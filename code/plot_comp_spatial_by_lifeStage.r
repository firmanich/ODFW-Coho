plot_comp_spatial_by_lifeStage <- function(plot_year = c(1998,2010,2019),
                                           frms = c("covar_only","spatial_only","best_mod"),
                                           stages=c("rear"),
                                           myFacet = formula(gsub("[\r\n\t]", "","LifeStage + yr + frm ~ mod")),
                                           save_to_file = FALSE,
                                           n_years_ahead = 0,
                                           test_years = 2019){

  library(sp)
  library(glmmTMB)
  library(raster)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  library(cowplot)
  source("code/wrangle_data.r")
  source('code/model_args.r')
  mod_search <- model_args(n_years_ahead = 0,
                           test_years = 2019,
                           no_covars = FALSE)
  
  j <- 1
  for(stage in stages){
    load(paste0("output/output_",stage,".rData"))
    df <- wrangle_data(stage=stage)
    for(f in frms){
      cat(f)
      cat(stage)
      cat("/n")
      #Grab the data for the stage    
      
      #Create projection grid
      x_seq <- seq(min(df$UTM_E_km),max(df$UTM_E_km)*1.1,by = 2) #UTM N
      y_seq <- seq(min(df$UTM_N_km),max(df$UTM_N_km)*1.1,by = 5) #UTM N
      vizloc_xy = expand.grid( x=x_seq, y=y_seq) #Spatial field
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
      
      
      #2c) Create the OC ESU mask
      myMask <- mask(rasterDF,SpP)
      tmp <- as.data.frame(cbind(coordinates(myMask),p = myMask@data@values))
      predmask <- na.omit(tmp)
      
      #2e) Now you need the covariates for your model
      p.df <- data.frame("UTM_E_km" = predmask$x, "UTM_N_km" = predmask$y) #start with population groups
      p.df$UTM_E <- p.df$UTM_E_km*1000
      p.df$UTM_N <- p.df$UTM_N_km*1000
      p.df[,names(df)[!names(df)%in%names(p.df)]] <- 0 #add all of the column heading
      p.df <- do.call("rbind", replicate(length(unique(df$yr)), p.df, simplify = FALSE))
      #Use kmeans to map the stream in the original data frame to 
      #the prediction dataframe
      kmean <- nn2(df[,c('UTM_E','UTM_N')],p.df[,c('UTM_E','UTM_N')], k=1)
      
      #interpolate covariates with kmeans distances
      if(f=="covar_only"){
        coVars <- c('WidthM','W3Dppt',
                    'MWMT_Index','StrmPow',
                    'SprPpt','IP_COHO', 
                    'SolMean','StrmSlope',
                    'OUT_DIST'
        )
        for(iii in coVars)
          p.df[,iii] <- df[kmean$nn.idx,iii]
      }
      
      #You always need to do this strm_order step.
      p.df[,'STRM_ORDER'] <- df[kmean$nn.idx,'STRM_ORDER']
      p.df$fSTRM_ORDER <- as.factor(1)
      # p.df$fSTRM_ORDER <- as.factor(p.df$STRM_ORDER)
      p.df$yr <- rep(unique(df$yr),each=nrow(predmask))
      p.df$fYr <- as.factor(p.df$yr)
      p.df$LifeStage <- stage
      
      for(k in c("gam","sdm","rf")){
        print(k)
        #default fit is the best fit to the data
        fit <- output$exploratory[[k]]$best_fit
        
        if(k=="gam"){
          if(f=="spatial_only"){
            ff <- mod_search$form$gam$m1
            fit <-  gam(ff, data=df, family = "tw")
          }
          if(f=="covar_only"){
            ff <- mod_search$form$gam$m3
            fit <-  gam(ff, data=df, family = "tw")
          }
          print(ff)
          print(fit)
          print(dim(p.df[p.df$yr%in%plot_year,]))
          pred <- exp(predict(fit, p.df[p.df$yr%in%plot_year,]))
          if(j==1){
            print(j)
            p <- cbind(p.df[p.df$yr%in%plot_year,],pred)
            p$mod <- "GAM \n (mgcv)"
            p$frm <- f
          }else{
            tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred)
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
          pred <- predict(fit, p.df[p.df$yr%in%plot_year,])
          tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred)
          tmp$mod <- "Random forest \n (randomForest)"
          tmp$frm <- f
          p <- rbind(p,tmp)
        }
        if(k=="sdm"){
          mesh <- make_mesh(df, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
          if(f=="spatial_only"){
            ff <- mod_search$form$sdm$m1
            fit <- sdmTMB(ff,
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
            ff <- mod_search$form$sdm$m2
            fit <- sdmTMB(ff,
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
          pred<-exp(predict(fit, p.df)$est)
          tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred[p.df$yr%in%plot_year])
          names(tmp)[ncol(tmp)] <- "pred"
          tmp$mod <- "GLMM \n (sdmTMB)"
          tmp$frm <- f
          p <- rbind(p,tmp)
        }
        j <- j + 1
      }
    }
  }#end form
  
  lr <- log(range(c(p$pred[p$yr%in%plot_year])))
  g <- ggplot(p[p$yr%in%plot_year,],
               aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
               alpha=0.2) +
    geom_raster(show.legend = FALSE) +
    facet_grid(myFacet) +
    # scale_fill_continuous()+
    scale_fill_viridis_c(option="inferno")+#,
                         # limits=lr,
                         # breaks=seq(min(lr),max(lr),length.out=10)
    # ) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
    # scale_color_viridis(limits=c(-7,6), breaks=seq(-7,6,by=2), discrete = FALSE) +
    ylab("Northing (km)") +
    xlab("Easting (km)")
  # assign(paste0("g",j),g)
#   
#   g <- ggplot(p[p$yr%in%plot_year,],
#                aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
#                alpha=0.2) +
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
#           panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#     geom_point(data=df[df$yr%in%plot_year & df$dens>0,],
#                aes(x=UTM_E_km,y=UTM_N_km, col=log(dens)),
#                inherit.aes = FALSE,
#                size=4,
#                alpha=0.9) +
#     geom_point(data=df[df$yr%in%plot_year & df$dens==0 & df$UTM_E_km>0,],
#                aes(x=UTM_E_km,y=UTM_N_km),
#                inherit.aes = FALSE,
#                color="black",
#                shape=1,
#                size=4,
#                alpha=0.9) +
#     labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
#     scale_color_viridis_c(option="inferno",
#                           limits=lr,
#                         breaks=seq(floor(min(lr)),ceiling(max(lr)),by=2)) +
#     ylab("Northing (km)") +
#     xlab("Easting (km)")
#   assign(paste0("g",j),g)
#   j <- j + 1
# 
  if(save_to_file)
    png('output/plot_comp_spatial_by_lifeStage.png',
        height = 800, width = 800)
# g <- plot_grid(g1, g2, g3, g4, 
#                labels = c('A', 'B','C','D'), 
#                label_size = 12, ncol=2)
# 
  print(g)

  if(save_to_file)
    dev.off()
  
return(p)
  
}
