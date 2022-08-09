spatial_index <- function(arg_no_covars=TRUE){
  library(sp)
  library(glmmTMB)
  library(raster)
  library(ggplot2)
  library(viridis)
  library(viridisLite)
  library(cowplot)
  
  
  #2e) Now you need the covariates for your model
  png('output/spatial_index.png',
      height = 800, width = 800)
  
  stage <- "Spwn" #rear or Spwn
  df <- wrangle_data(stage=stage)
  if(arg_no_covars){
    load(paste0("output/output_st_",stage,".rData"))
  }else{
    load(paste0("output/output_",stage,".rData"))
  }
  
  #Subset by unique locations, non-duplicated df
  nd.df <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km')]),c('UTM_E_km','UTM_N_km')]
  nd <- nrow(nd.df)
  nd.df$UTM_E <- nd.df$UTM_E_km*1000
  nd.df$UTM_N <- nd.df$UTM_N_km*1000
  nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
  nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
  nd.df$STRM_ORDER <- 1
  nd.df$fSTRM_ORDER <- as.factor(1)
  nd.df$yr <- rep(unique(df$yr),each=nd)
  nd.df$fYr <- as.factor(nd.df$yr)
  
  
  fit <- output$exploratory$gam$best_fit
  # pred <- exp(predict(fit, df))#
  pred1 <- exp(predict(fit, nd.df))
  p <- cbind(nd.df,pred1)
  # p <- cbind(df,pred) 
  p$mod <- "GAM \n (mgcv)"
  
  fit <- output$exploratory$rf$best_fit
  pred1 <- predict(fit, nd.df)
  tmp <- cbind(nd.df,pred1)
  tmp$mod <- "Random forest \n (randomForest)"
  p <- rbind(p,tmp)
  # 
  
  fit <- output$exploratory$sdm$best_fit
  pred1 <-exp(predict(fit, nd.df)$est)
  # tmp <- cbind(nd.df,pred$est)
  tmp <- cbind(nd.df,pred1)
  names(tmp)[ncol(tmp)] <- "pred1"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)
  
  ag <- aggregate(list(est = p$pred1), by=list(yr = p$yr), sum)
  par(mfrow=c(2,2))
  odfw_est <- read.csv("ESU_Estimates.csv")
  if(stage=="Spwn"){
    odfw <- data.frame(yr=odfw_est$Brood.Year,
                       odfw=odfw_est$Spawners)
  }
  if(stage=="rear"){
    odfw <- data.frame(yr=odfw_est$Parr.Year,
                       odfw=odfw_est$TotalParr)
  }
  
  ag$odfw <- odfw$odfw[odfw$yr%in%ag$yr]
  
  non_dup <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km')]),c('UTM_E_km','UTM_N_km')]
  plot(non_dup$UTM_E_km,non_dup$UTM_N_km,
       xlab="Easting",
       ylab="Northing")
  matplot(ag$yr,
          ag[,2:3], 
          type="l",
          xlab="Year",
          ylab=paste(stage,"index"))
  legend(max(ag$yr)*0.7,
         max(ag[,2:3])*0.9, 
         legend=names(ag[,2:3]),
         lty=1:2,
         col=1:2)
  
  
  #2e) Now you need the covariates for your model
  stage <- "rear" #rear or Spwn
  df <- wrangle_data(stage=stage)
  if(no_covars){
    load(paste0("output/output_st_",stage,".rData"))
  }else{
    load(paste0("output/output_",stage,".rData"))
  }
  
  #Subset by unique locations, non-duplicated df
  nd.df <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km')]),c('UTM_E_km','UTM_N_km')]
  nd <- nrow(nd.df)
  nd.df$UTM_E <- nd.df$UTM_E_km*1000
  nd.df$UTM_N <- nd.df$UTM_N_km*1000
  nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
  nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
  nd.df$STRM_ORDER <- 1
  nd.df$fSTRM_ORDER <- as.factor(1)
  nd.df$yr <- rep(unique(df$yr),each=nd)
  nd.df$fYr <- as.factor(nd.df$yr)
  
  
  fit <- output$exploratory$gam$best_fit
  # pred <- exp(predict(fit, df))#
  pred1 <- exp(predict(fit, nd.df))
  p <- cbind(nd.df,pred1)
  # p <- cbind(df,pred) 
  p$mod <- "GAM \n (mgcv)"
  
  fit <- output$exploratory$rf$best_fit
  pred1 <- predict(fit, nd.df)
  tmp <- cbind(nd.df,pred1)
  tmp$mod <- "Random forest \n (randomForest)"
  p <- rbind(p,tmp)
  # 
  
  fit <- output$exploratory$sdm$best_fit
  pred1 <-exp(predict(fit, nd.df)$est)
  # tmp <- cbind(nd.df,pred$est)
  tmp <- cbind(nd.df,pred1)
  names(tmp)[ncol(tmp)] <- "pred1"
  tmp$mod <- "GLMM \n (sdmTMB)"
  p <- rbind(p,tmp)
  
  ag <- aggregate(list(est = p$pred1), by=list(yr = p$yr), sum)
  odfw_est <- read.csv("ESU_Estimates.csv")
  if(stage=="Spwn"){
    odfw <- data.frame(yr=odfw_est$Brood.Year,
                       odfw=odfw_est$Spawners)
  }
  if(stage=="rear"){
    odfw <- data.frame(yr=odfw_est$Parr.Year,
                       odfw=odfw_est$TotalParr)
  }
  
  ag$odfw <- odfw$odfw[odfw$yr%in%ag$yr]
  
  non_dup <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km')]),c('UTM_E_km','UTM_N_km')]
  plot(non_dup$UTM_E_km,non_dup$UTM_N_km,
       xlab="Easting",
       ylab="Northing")
  matplot(ag$yr,
          ag[,2:3], 
          type="l",
          xlab="Year",
          ylab=paste(stage,"index"))
  legend(max(ag$yr)-10,
         max(ag[,2:3]), 
         legend=names(ag[,2:3]),
         lty=1:2,
         bg="transparent",
         col=1:2)
  
  
  dev.off()
  
}
