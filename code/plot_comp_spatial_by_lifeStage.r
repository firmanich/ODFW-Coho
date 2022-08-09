plot_comp_spatial_by_lifeStage <- function(plot_year = 2019, arg_no_covars = TRUE){

  library(sp)
  library(glmmTMB)
  library(raster)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  library(cowplot)
  #Predict the spatiotemporal effects for plot year and covars(?)

  # plot_year = 1998
  # arg_no_covars = TRUE
  #Before we can plot the predictions, we have to do a couple of this
  #1) Create the grid,
  #1a) Create the grid
  source("wrangle_data.r")
  stage <- "rear" #rear or Spwn
  df <- wrangle_data(stage=stage)
  
  x_seq <- seq(min(df$UTM_E_km),max(df$UTM_E_km)*1.1,by = 2) #UTM N
  y_seq <- seq(min(df$UTM_N_km),max(df$UTM_N_km)*1.1,by = 5) #UTM N
  vizloc_xy = expand.grid( x=x_seq, y=y_seq) #Spatial field
  vizloc_xy$p <- 1
  #1b) transform gridded data into coordinates and raster
  coordinates(vizloc_xy) <- ~x+y
  # coerce to SpatialPixelsDataFrame
  gridded(vizloc_xy) <- TRUE
  # coerce to raster
  rasterDF <- raster(vizloc_xy)
  
  #2) Remove all grid cells from rasterDF that are outside of OC ESU
  #2a) Get Oregon spatial ESU
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
  # plot(predmask$x,predmask$y)
  
  
  #2e) Now you need the covariates for your model
  p.df <- data.frame("UTM_E_km" = predmask$x, "UTM_N_km" = predmask$y) #start with population groups
  p.df$UTM_E <- p.df$UTM_E_km*1000
  p.df$UTM_N <- p.df$UTM_N_km*1000
  p.df[,names(df)[!names(df)%in%names(p.df)]] <- 0 #add all of the column heading
  p.df <- do.call("rbind", replicate(length(unique(df$yr)), p.df, simplify = FALSE))
  p.df$STRM_ORDER <- 1
  p.df$fSTRM_ORDER <- as.factor(1)
  p.df$yr <- rep(unique(df$yr),each=nrow(predmask))
  p.df$fYr <- as.factor(p.df$yr)
  
  if(arg_no_covars){
    load(paste0("output/output_st_",stage,".rData"))
  }else{
    load(paste0("output/output_",stage,".rData"))
  }
  
  fit <- output$exploratory$gam$best_fit
  pred <- exp(predict(fit, p.df[p.df$yr%in%plot_year,]))
  p <- cbind(p.df[p.df$yr%in%plot_year,],pred)
  p$mod <- "GAM \n (mgcv)"
  
  fit <- output$exploratory$rf$best_fit
  pred <- predict(fit, p.df[p.df$yr%in%plot_year,])
  tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred)
  tmp$mod <- "Random forest \n (randomForest)"
  p <- rbind(p,tmp)
  # 
  
  fit <- output$exploratory$sdm$best_fit
  pred<-exp(predict(fit, p.df)$est)
  tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred[p.df$yr%in%plot_year])
  names(tmp)[ncol(tmp)] <- "pred"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)
  
  p_tmp <- p %>%
    gather(cat,'Density(#/km)',dens,pred)

  g1 <- ggplot(p[p$yr%in%plot_year,],
               aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
               alpha=0.2) +
    geom_raster(show.legend = FALSE) +
    facet_grid(~mod) +
    # scale_fill_continuous()+
    scale_fill_viridis_c(limits=c(0,9), breaks=seq(0,9,by=2)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
    scale_color_viridis(limits=c(0,9), breaks=seq(0,9,by=2), discrete = FALSE) +
    ylab("Northing (km)") +
    xlab("Easting (km)")

  g2 <- ggplot(p[p$yr%in%plot_year,],
               aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
               alpha=0.2) +
    scale_fill_viridis_c(limits=c(0,9), breaks=seq(0,9,by=2)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_point(data=df[df$yr%in%plot_year & df$dens>0,],
               aes(x=UTM_E_km,y=UTM_N_km, color=log(dens)),
               inherit.aes = FALSE,
               size=4,
               alpha=0.9) +
    geom_point(data=df[df$yr%in%plot_year & df$dens==0,],
               aes(x=UTM_E_km,y=UTM_N_km),
               inherit.aes = FALSE,
               color="black",
               shape=1,
               size=4,
               alpha=0.9) +
    labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
    scale_color_viridis(limits=c(0,max(ceiling(log(df$dens[df$LifeStage==stage & df$dens>0])))),
                        breaks=seq(0,max(ceiling(log(df$dens[df$LifeStage==stage & df$dens>0]))),by=2),
                        discrete = FALSE) +
    ylab("Northing (km)") +
    xlab("Easting (km)")


  stage <- "Spwn" #rear or Spwn
  df <- wrangle_data(stage=stage)
  #2e) Now you need the covariates for your model
  p.df <- data.frame("UTM_E_km" = predmask$x, "UTM_N_km" = predmask$y) #start with population groups
  p.df$UTM_E <- p.df$UTM_E_km*1000
  p.df$UTM_N <- p.df$UTM_N_km*1000
  p.df[,names(df)[!names(df)%in%names(p.df)]] <- 0 #add all of the column heading
  p.df <- do.call("rbind", replicate(length(unique(df$yr)), p.df, simplify = FALSE))
  p.df$STRM_ORDER <- 1
  p.df$fSTRM_ORDER <- as.factor(1)
  p.df$yr <- rep(unique(df$yr),each=nrow(predmask))
  p.df$fYr <- as.factor(p.df$yr)
  
  if(arg_no_covars){
    load(paste0("output/output_st_",stage,".rData"))
  }else{
    load(paste0("output/output_",stage,".rData"))
  }


  fit <- output$exploratory$gam$best_fit
  # pred <- exp(predict(fit, df))#
  pred <- exp(predict(fit, p.df[p.df$yr%in%plot_year,]))
  p <- cbind(p.df[p.df$yr%in%plot_year,],pred)
  p$mod <- "GAM \n (mgcv)"

  fit <- output$exploratory$rf$best_fit
  pred <- predict(fit, p.df[p.df$yr%in%plot_year,])
  tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred)
  tmp$mod <- "Random forest \n (randomForest)"
  p <- rbind(p,tmp)
  #

  fit <- output$exploratory$sdm$best_fit
  pred<-exp(predict(fit, p.df)$est)
  tmp <- cbind(p.df[p.df$yr%in%plot_year,],pred[p.df$yr%in%plot_year])
  names(tmp)[ncol(tmp)] <- "pred"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)

  p_tmp <- p %>%
    gather(cat,'Density(#/km)',dens,pred)

  g3 <- ggplot(p[p$yr%in%plot_year,],
               aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
               alpha=0.2) +
    geom_raster(show.legend = FALSE) +
    facet_grid(~mod) +
    # scale_fill_continuous()+
    scale_fill_viridis_c(limits=c(-7,6), breaks=seq(-7,9,by=2)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
    scale_color_viridis(limits=c(-7,6), breaks=seq(-7,6,by=2), discrete = FALSE) +
    ylab("Northing (km)") +
    xlab("Easting (km)")

  g4 <- ggplot(p[p$yr%in%plot_year,],
               aes(x=UTM_E_km,y=UTM_N_km, fill=log(pred)),
               alpha=0.2) +
    scale_fill_viridis_c(limits=c(-7,6), breaks=seq(-7,6,by=2)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))+
    geom_point(data=df[df$yr%in%plot_year & df$dens>0,],
               aes(x=UTM_E_km,y=UTM_N_km, color=log(dens)),
               inherit.aes = FALSE,
               size=4,
               alpha=0.9) +
    geom_point(data=df[df$yr%in%plot_year & df$dens==0,],
               aes(x=UTM_E_km,y=UTM_N_km),
               inherit.aes = FALSE,
               color="black",
               shape=1,
               size=4,
               alpha=0.9) +
    labs(fill = "log(#/km^2)\n", color = "log(# / km^2)\n")+
    scale_color_viridis(limits=c(-7,6), breaks=seq(-7,6,by=2), discrete = FALSE) +
    ylab("Northing (km)") +
    xlab("Easting (km)")

  png('output/plot_comp_spatial_by_lifeStage.png',
      height = 800, width = 800)
  g <- plot_grid(g1, g2,g3,g4, 
                 labels = c('A', 'B','C','D'), 
                 label_size = 12, nrow=2)
  print(g)
  dev.off()
  
}
