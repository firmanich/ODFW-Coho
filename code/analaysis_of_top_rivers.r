#First fit to all of the data.
#from 1998 to 

mape <- function(est,obs){return(sum((est-obs)/obs)/length(obs))}

forecast_yrs <- 2015:2019

mape_top_pred <- forecast_yrs
mape_rand_pred <- forecast_yrs

life_stage <- 'rear'
#Get the observed data
load(paste0("./output/output_",life_stage,".rData"))
obs_data <- output$exploratory$sdm$best_fit$data
fit <- output$exploratory$sdm$best_fit
output <- data.frame(sample = NA, forecast_yrs = NA, mape_top_pred = NA, mape_rand_pred = NA)

icnt <- 1
nrep <- 2

for(j in 1:nrep){
  for(i in forecast_yrs){
    print(i)
    forecast_yr <- i
    
    
    burn_yrs <- obs_data[obs_data$yr <= 2014,]
    burn_yrs$fYr <- as.factor(burn_yrs$yr)
    
    top_pops <- c("Coos","Coquillle","Tillamook Bay", "Lower Umpqua", "Alsea","Nestucca")
    top_pop_data <- obs_data[(obs_data$PopGrp%in%top_pops) & obs_data$yr%in%(2015:forecast_yr),]
    top_pop_data$fYr <- as.factor(top_pop_data$yr)
    top_data <- rbind(burn_yrs,top_pop_data)
    
    if(i == 2015){
      rand_pop <- sample(1:24, size = 6)
    }
    rand_pops <- unique(obs_data$PopGrp)[rand_pop]
    
    rand_pop_data <- obs_data[(obs_data$PopGrp%in%rand_pops) & obs_data$yr%in%(2015:forecast_yr),]
    rand_pop_data$fYr <- as.factor(rand_pop_data$yr)
    rand_data <- rbind(burn_yrs,rand_pop_data)
    
    
    pred_data <- obs_data[(obs_data$PopGrp%in%top_pops)==FALSE & obs_data$yr%in%(2015:forecast_yr),]
    pred_data$fYr <- as.factor(pred_data$yr)
    
    
    rand_pred_data <- obs_data[(obs_data$PopGrp%in%rand_pops)==FALSE & obs_data$yr%in%(2015:forecast_yr),]
    rand_pred_data$fYr <- as.factor(rand_pred_data$yr)
    
    mesh <- make_mesh(top_data, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
    
    if(icnt ==1){
      fit <- sdmTMB(fit$formula[[1]],
                    data = top_data,
                    mesh = mesh,
                    family = tweedie(link = "log"),
                    time = "yr",
                    spatial = TRUE,
                    spatiotemporal = TRUE,
                    anisotropy = TRUE, #Why is this necessary if the years are the same?
                    silent=TRUE)
      top_pred <- predict(fit, newdata = rbind(burn_yrs,pred_data))
      top_pred_oob <- top_pred[!(top_pred$PopGrp%in%top_pops) & top_pred$yr==forecast_yr,]
      
      mape_top_pred[icnt] <- mape(top_pred_oob$dens,exp(top_pred_oob$est))
    }
    
    
    mesh <- make_mesh(rand_data, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
    fit <- sdmTMB(fit$formula[[1]],
                  data = rand_data,
                  mesh = mesh,
                  family = tweedie(link = "log"),
                  time = "yr",
                  spatial = TRUE,
                  spatiotemporal = TRUE,
                  anisotropy = TRUE, #Why is this necessary if the years are the same?
                  silent=TRUE)
    rand_pred <- predict(fit, newdata = rbind(burn_yrs,rand_pred_data))
    rand_pred_oob <- rand_pred[(rand_pred$PopGrp%in%top_pops)==FALSE & rand_pred$yr==forecast_yr,]
    mape_rand_pred[icnt] <- mape(rand_pred_oob$dens,exp(rand_pred_oob$est))
    
    icnt <- icnt + 1  
  }
  
  output <- rbind(output,cbind(sample=rep(j,5),forecast_yrs,mape_top_pred,mape_rand_pred))
}

