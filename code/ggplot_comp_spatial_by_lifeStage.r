plot_comp_spatial_by_lifeStage <- function(plot_year = c(1998,2010,2019),
                                           frms = c("covar_only","spatial_only","best_mod"),
                                           stages=c("rear","Spwn"), #rear and or Spwn
                                           myFacet = formula(gsub("[\r\n\t]", 
                                                                  "",
                                                                  "LifeStage + yr~ mod")),
                                           # myFacet = formula(gsub("[\r\n\t]", 
                                           #                        "",
                                           #                        "LifeStage + yr + frm ~ mod")),
                                           save_to_file = FALSE,
                                           n_years_ahead = 0,
                                           comp_mods = c("gam","sdm","rf"),
                                           save_raster = FALSE,
                                           n_test = 1,
                                           UTM_N_step = 1,
                                           UTM_E_step = 1){

  library(sp)
  library(dplyr)
  library(tidyr)
  library(sdmTMB)
  library(randomForest)
  library(mgcv)
  library(raster)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  library(cowplot)
  library(RANN)
  source("./code/function_model_search.r")
  source("./code/function_wrangle_data.r")
  j <- 1
    # stages <- "rear"
  for(stage in stages){
    #These are the tested years, as opposed to the training years
    # stage <- "rear", "Spwn"
    load(file = paste0("C:/NOAA/PROJECTS/ODFW-Coho/output/output_",stage,".rdata"))
    
    #Grab the data for stage
    df <- function_wrangle_data(stage=stage,
                                dir = root)
    
    test_years = seq(max(df$yr)-n_test+1, max(df$yr))
    
    #tagged list of model arguments and models to search over
    n_years_ahead <- 0
    project <- TRUE
    no_covars <- FALSE
    mod_search <- function_model_search(test_years = test_years,
                                        n_years_ahead = n_years_ahead,
                                        project = project,
                                        no_covars = no_covars,
                                        output = output)
    for(f in frms){
      cat(f)
      cat(stage)
      cat("/n")

      #Create projection grid
      x_seq <- seq(min(df$UTM_E_km),
                   max(df$UTM_E_km)*1.1,
                   by = UTM_E_step) #UTM N
      y_seq <- seq(min(df$UTM_N_km),
                   max(df$UTM_N_km)*1.1,
                   by = UTM_N_step) #UTM N
      vizloc_xy = expand.grid( x = x_seq, 
                               y = y_seq) #Spatial field
      vizloc_xy$p <- 1
      
      # transform gridded data into coordinates and raster
      coordinates(vizloc_xy) <- ~x+y
      # coerce to SpatialPixelsDataFrame
      gridded(vizloc_xy) <- TRUE
      # coerce to raster
      rasterDF <- raster(vizloc_xy)
      
      # Remove all grid cells from rasterDF that are outside of OC ESU
      # Get Oregon spatial ESU
      dir <- "C:/NOAA/PROJECTS/ODFW-Coho/"
      setwd(paste0(dir,"shp/"))
      shape = rgdal::readOGR(".","OregonCoast_coho_ESU-utm83") #will load the shapefile to your dataset.
      OR_ESU <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
      names(OR_ESU) <- c("X","Y")
      
      setwd(dir) #restore root directory
      coastXY <- data.frame(X=OR_ESU$X,Y=OR_ESU$Y)/1000 #coastline polygon
      Sr1 = Polygon(cbind(OR_ESU$X/1000,OR_ESU$Y/1000))
      SpP = SpatialPolygons(list(Polygons(list(Sr1), "s1")), 1:1) #1:1 is the trick, can't say 1
      
      
      #Oregon map  
      or <- map_data("state","oregon")
      or_utm <- sdmTMB::add_utm_columns(data.frame("longitude"=or$long,"latitude"=or$lat))
      
      
      #2) Create the OC ESU mask
      myMask <- mask(rasterDF,SpP)
      
      tmp <- as.data.frame(cbind(coordinates(myMask),
                                 p = myMask@data@values))
      predmask <- na.omit(tmp)
      
      #2b) Now you need the covariates for your model
      p.df <- data.frame("UTM_E_km" = predmask$x, 
                         "UTM_N_km" = predmask$y) #start with population groups
      p.df$UTM_E <- p.df$UTM_E_km*1000
      p.df$UTM_N <- p.df$UTM_N_km*1000
      
      #Start off with everything equal to zero
      p.df[,names(df)[!names(df)%in%names(p.df)]] <- 0 #add all of the column heading
      
      #replicate by number of years
      p.df <- do.call("rbind", 
                      replicate(length(unique(df$yr)), 
                                p.df, simplify = FALSE))
      
      p.df$yr <- rep(unique(df$yr),each=nrow(predmask))
      p.df$fYr <- as.factor(p.df$yr)
      #Use kmeans to map the stream in the original data frame to 
      #the prediction dataframe
      kmean <- nn2(df[,c('UTM_E','UTM_N')],p.df[,c('UTM_E','UTM_N')], k=1)
      
      #interpolate covariates with kmeans distances
      if(f!="spatial_only"){
        coVars <- c('WidthM','W3Dppt',
                    'MWMT_Index','StrmPow',
                    'SprPpt','IP_COHO','STRM_ORDER', 
                    'SolMean','StrmSlope',
                    'OUT_DIST'
        )
        
        # There's an error here
        for(iii in coVars){
          for(y in unique(p.df$yr)){
            kmean <- nn2(df[df$yr==y,c('UTM_E','UTM_N')],p.df[p.df$yr==y,c('UTM_E','UTM_N')], k=1)
            p.df[p.df$yr==y,iii] <- df[kmean$nn.idx,iii]
          }
        }
      }
      
      #You always need to do this strm_order step.
      # p.df[,'STRM_ORDER'] <- df[kmean$nn.idx,'STRM_ORDER']
      p.df$fSTRM_ORDER <- as.factor(p.df$STRM_ORDER)
      p.df$LifeStage <- stage
      ki <- 1
      for(k in comp_mods){
        print(k)
        #default fit is the best fit to the data
        fit <- output$exploratory[[k]]$best_fit
        
        if(k=="gam"){
          if(f=="spatial_only"){
            fit <-  gam(mod_search$form$gam$m1, 
                        data=df, 
                        family = "tw")
          }
          if(f=="covar_only"){
            fit <-  gam(mod_search$form$gam$m3, 
                        data=df, 
                        family = "tw")
          }
          pred <- exp(predict(fit, p.df[p.df$yr%in%plot_year,]))
          if(j==1){
            p <- cbind(p.df[p.df$yr%in%plot_year,],
                       pred)
            p$mod <- "GAM \n (mgcv)"
            p$frm <- f
            j <- j + 1
          }else{
            tmp <- cbind(p.df[p.df$yr%in%plot_year,],
                         pred)
            tmp$mod <- "GAM \n (mgcv)"
            tmp$frm <- f
            p <- rbind(p,tmp)
          }
        }
        if(k=="rf"){
          if(f=="spatial_only"){
            ff <- mod_search$form$rf$m1
            mtry <- 3 #There are only three variable, utm_n, utm_e, yr
            ntree <- output$exploratory$rf$best_ntree
          }
          if(f=="covar_only"){
            ff <- mod_search$form$rf$m2
            mtry <- output$exploratory$rf$best_mtry
            ntree <- output$exploratory$rf$best_ntree
          }
          if(f=="best_mod"){
            ff <- output$exploratory$rf$best_mod
            mtry <- output$exploratory$rf$best_mtry
            ntree <- output$exploratory$rf$best_ntree
          }
          fit <- randomForest(ff,
                              mtry = mtry,
                              ntree = ntree,
                              data = df)
          if(j==1){
            pred <- predict(fit, p.df[p.df$yr%in%plot_year,])
            p <- cbind(p.df[p.df$yr%in%plot_year,],
                       pred)
            p$mod <- "Random forest \n (randomForest)"
            p$frm <- f
            j <- j + 1
          }else{
            pred <- predict(fit, p.df[p.df$yr%in%plot_year,])
            tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred)
            tmp$mod <- "Random forest \n (randomForest)"
            tmp$frm <- f
            p <- rbind(p,tmp)
          }
        }
        if(k=="sdm"){
          mesh <- make_mesh(df, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
          if(f=="spatial_only"){
            fit <- sdmTMB(mod_search$form$sdm$m1,
                          data = df,
                          mesh = mesh,
                          family = tweedie(link = "log"),
                          time = "yr",
                          spatial = TRUE,
                          spatiotemporal = TRUE,
                          anisotropy = TRUE,
                          # extra_time = unique(test$yr[test$yr>max(train$yr)]), #Why is this necessary if the years are the same?
                          silent=TRUE)

          }
          if(f=="covar_only"){
            fit <- sdmTMB(mod_search$form$sdm$m2,
                          data = df,
                          mesh = mesh,
                          family = tweedie(link = "log"),
                          time = "yr",
                          spatial = FALSE,
                          spatiotemporal = FALSE,
                          anisotropy = FALSE,
                          # extra_time = unique(test$yr[test$yr>max(train$yr)]), #Why is this necessary if the years are the same?
                          silent=TRUE)

          }
          if(j==1){
            pred<-exp(predict(fit, p.df)$est)[p.df$yr%in%plot_year]
            p <- cbind(p.df[p.df$yr%in%plot_year,],
                       pred)
            p$mod <- "GLMM \n (sdmTMB)"
            p$frm <- f
            j <- j + 1
          }else{
            pred<-exp(predict(fit, p.df)$est)
            tmp <- cbind(p.df[p.df$yr%in%plot_year,],
                         pred[p.df$yr%in%plot_year])
            names(tmp)[ncol(tmp)] <- "pred"
            tmp$mod <- "GLMM \n (sdmTMB)"
            tmp$frm <- f
            p <- rbind(p,tmp)
          }
        }
      }#comp mods
    }#end frms 
  }#end stages
  
  lr <- log(range(c(p$pred[p$yr%in%plot_year])))

  p$LifeStage[p$LifeStage =='rear'] <- 'Juvenile'
  p$LifeStage[p$LifeStage =='Spwn'] <- 'Spawner'

  g <- ggplot(p[p$yr%in%plot_year,],
               aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
               alpha=0.2) +
    geom_raster(show.legend = TRUE) +
    facet_grid(myFacet) +
    # scale_fill_continuous()+
    scale_fill_viridis_c(option="inferno")+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"))+
    labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
    ylab("Northing (km)") +
    xlab("Easting (km)")

  if(save_to_file)
    tiff(paste0('output/plot_comp_spatial_by_lifeStage_',stage,'.tiff'),
        height = 600, width = 600, pointsize = 14)

  print(g)

  if(save_to_file)
    dev.off()

  tmp_ras <- (p[,c('UTM_E_km','UTM_N_km','pred')])
  names(tmp_ras) <- c('x','y','z')
  coordinates(tmp_ras) <- ~x+y
  gridded(tmp_ras) <- TRUE
  tmp_ras <- raster(tmp_ras)

  if(save_raster){
    tiff(file = paste0("./output/",stages,comp_mods,frms,plot_year,".tiff"))
    plot(tmp_ras)
    dev.off()
  }
  
  return(list(
              tmp_raster = tmp_ras,
              tmp = tmp,
              p = p,
              p.df = p.df,
              g = g
              )
         )
  
}
