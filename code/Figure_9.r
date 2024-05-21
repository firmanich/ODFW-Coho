#Get the observed data
load("./output/output_rear.rData")
obs_data <- output$exploratory$sdm$best_fit$data
mod <- output$exploratory$sdm$best_mod
st <- output$exploratory$sdm$best_st
sp <- output$exploratory$sdm$best_sp
  

high_rivers_rear <- c("Nestucca", "Siuslaw", 'Lower Umpqua', 'Coos', "South Umpqua")
med_rivers_rear <- c("Beaver", "Siltcoos", 'Alsea', 'Necanicum', "Salmon")
rear_list <- list(high_rivers_rear = high_rivers_rear,
                  med_rivers_rear = med_rivers_rear)

icnt <- 1
rivers <- unique(obs_data$PopGrp)
rivers <- rivers[!(1:length(rivers))%in%grep("Dependent", rivers)]

if(recreate_analysis){
  yrs <- 2017:2021
  for(yr_i in yrs){
    
    #Fit to the full data set
    obs_mesh <- sdmTMB::make_mesh(obs_data[obs_data$yr <= yr_i,], 
                                  xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
    obs_fit <- sdmTMB(mod, 
                      data = obs_data[obs_data$yr <= yr_i,],
                      mesh = obs_mesh,
                      family = tweedie(link = "log"),
                      time = "yr",
                      spatial = sp,
                      spatiotemporal = st,
                      anisotropy = TRUE,
                      # extra_time = unique(obs_data$yr[obs_data$yr <= yr_i]), #Why is this necessary if the years are the same?
                      silent=TRUE)
    
    
    #loop over the rivers for the year of interest  
    for(rr in rivers){
      
      print(paste(rr,yr_i))
      #Make sure that the year of intersest has been sample for this river
      if(yr_i%in%unique(obs_data$yr[obs_data$PopGrp==rr])){
        
        #create the prediction data set
        pred_data <- obs_data[obs_data$yr <= yr_i, ]
        #fill in missing years with dummy data
        missing_years <- !(unique(pred_data[,]$yr)%in%unique(pred_data[pred_data$PopGrp==rr,]$yr))
        if(sum(missing_years)>0){
          missing_pred <- do.call(rbind,lapply(1:sum(missing_years==TRUE), function(x) pred_data[pred_data$PopGrp==rr,][1,]))
          missing_pred$yr <- unique(pred_data[,]$yr)[missing_years==TRUE]
          missing_pred[,6:17] <- 0
          pred_data <- rbind(pred_data,
                             missing_pred)
        }
        
        #Indexes for the full data      
        obs_pred <- predict(obs_fit, 
                            newdata = pred_data[pred_data$PopGrp==rr,], 
                            return_tmb_object=TRUE,
                            re_form_iid = NA)
        if(yr_i==min(yrs)){
          ind_with_full_data <- sdmTMB::get_index(obs_pred) %>% 
            filter(yr <= yr_i) %>% 
            mutate(data = "full",
                   cen_river = "full",
                   river = rr)
        }else{
          ind_with_full_data <- sdmTMB::get_index(obs_pred) %>% 
            filter(yr == yr_i) %>% 
            mutate(data = "full",
                   cen_river = "full",
                   river = rr)
        }
        
        for(cen_rivers in 1:length(rear_list)){
          #Create the censored dataset  
          data_before_year_of_interest <- obs_data[obs_data$yr < min(yrs), ]
          data_after_year_of_interest <- obs_data[obs_data$yr >= min(yrs) & obs_data$yr <= yr_i, ] %>% 
            filter((PopGrp %in% rear_list[[cen_rivers]]))
          
          censored_data <- rbind(data_before_year_of_interest,
                                 data_after_year_of_interest)
          
          #Fit to the cencored data
          cen_mesh <- sdmTMB::make_mesh(censored_data, 
                                        xy_cols = c("UTM_E_km","UTM_N_km"), cutoff = 10)
          cen_fit <- sdmTMB(mod, 
                            censored_data,
                            mesh = cen_mesh,
                            family = tweedie(link = "log"),
                            time = "yr",
                            spatial = sp,
                            spatiotemporal = st,
                            anisotropy = TRUE,
                            extra_time = unique(obs_data$yr[obs_data$yr <= yr_i]), #Why is this necessary if the years are the same?
                            silent=TRUE)
          #indexes for the censored data
          cen_pred <- predict(cen_fit, 
                              newdata = pred_data[pred_data$PopGrp==rr,], 
                              return_tmb_object=TRUE, 
                              re_form_iid = NA)
          ind_with_censored_data <- sdmTMB::get_index(cen_pred) %>% 
            filter(yr == yr_i) %>% 
            mutate(data = "censored",
                   cen_river = names(rear_list)[cen_rivers],
                   river = rr)
          
          #Store the output 
          if(icnt == 1){
            out <- rbind(ind_with_censored_data,
                         ind_with_full_data)
          }else{
            
            out <- rbind(out,
                         ind_with_censored_data,
                         ind_with_full_data)
          }
          icnt <- icnt + 1
        }
      }
    }
  }
}else{
  out <- readRDS("Figure_9_rear.rds")
}

list_num <- 2

out <- out %>%
  filter(yr >= 2017) #%>% 
# filter(river %in% c(unlist(rear_list[[list_num]]))) %>%
# mutate(cen_river = cen_river) 

river_order <- unique(c(high_rivers_rear, med_rivers_rear, unique(out$river[!(unique(out$river)%in%c(high_rivers_rear,med_rivers_rear))])))
# mutate(cen_river_label = factor(cen_river, labels = c("High \nRMSE", "Permanently\nremoved", "Medium \nRMSE"))) %>%
out <- out %>% 
  filter(yr >= 2017) %>% 
  mutate(river_labels = factor(river, levels = river_order)) %>%
  # mutate(cen_label = ifelse(river %in% rear_list[[1]], "High RMSE",
  #                           ifelse(river %in% rear_list[[2]], "Medium RMSE",
  #                                  "Permanently excluded")))  %>% 
  mutate(cen_label = ifelse(cen_river == names(rear_list)[1], paste0("\n",paste0(rear_list[[1]][1:2], collapse = ", "),"\n",
                                                                     paste0(rear_list[[1]][3], collapse = ", "),"\n",
                                                                     paste0(rear_list[[1]][4:5], collapse = ", ")),
                            ifelse(cen_river == names(rear_list)[2], paste0(rear_list[[2]], collapse = ", "),
                                   "All"))) %>%
  filter(cen_river != c("med_rivers_rear") ) #%>%
# na.omit() %>% 

obs <- obs_data %>% 
  filter(PopGrp %in% unique(out$river)) %>% 
  filter(yr >= 2017) %>% 
  group_by(PopGrp, yr) %>% 
  summarize( sum = sum(dens)) %>% 
  mutate(river = PopGrp)

out$sum <-   obs$sum[match(paste(out$river,out$yr), paste(obs$river, obs$yr))]

g <-   out %>% 
  # mutate(obs = obs$sum[match(river, obs$river)]) %>% 
  ggplot(aes(x = yr, y = est, group = cen_label, color = (cen_label))) +
  facet_wrap(~river_labels, ncol = 5, scales = "free_y") +
  geom_point(aes(),position = position_dodge(width = 0.5), size = 1.5, alpha = 0.5) +
  geom_errorbar(aes(ymin = lwr, ymax = upr, color = cen_label), position = position_dodge(width = 0.5)) +
  theme_bw() +
  xlab("Survey year") +
  labs(color = "Populations surveyed") +
  ylab("Cumulative number of spawners") +
  theme(panel.grid = element_blank(), ) +
  theme(text = element_text(size = 16)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_point(aes(x = yr, y = sum), color = "black", size = 1.5, shape = 16, alpha = 0.8)
print(g)
ggsave(filename = "./output/Figure_9.tiff", dpi = 300, units = "in", width = 12, height = 10)


#Out of bag samples censored analysis vs full data
out %>% filter(!(river %in% rear_list[[1]])) %>% filter(data == "censored") %>% summarize(sd_sd = sqrt(mean((est - sum)^2)))
out %>% filter(!(river %in% rear_list[[1]])) %>% filter(data == "full") %>% summarize(sd_sd = sqrt(mean((est - sum)^2)))

#Management strategy rivers, censored versus full
out %>% filter((river %in% rear_list[[1]])) %>% filter(data == "censored") %>% summarize(sd_sd = sqrt(mean((est - sum)^2)))
out %>% filter((river %in% rear_list[[1]])) %>% filter(data == "full") %>% summarize(sd_sd = sqrt(mean((est - sum)^2)))

#Standard error out of bag samples, censored versus full
out %>% filter(!(river %in% rear_list[[1]])) %>% filter(data == "censored") %>% summarize(sd_sd = mean(se))
out %>% filter(!(river %in% rear_list[[1]])) %>% filter(data == "full") %>% summarize(sd_sd = mean(se))

#Standard error management strategy population samples, censored versus full
out %>% filter((river %in% rear_list[[1]])) %>% filter(data == "censored") %>% summarize(sd_sd = mean(se))
out %>% filter((river %in% rear_list[[1]])) %>% filter(data == "full") %>% summarize(sd_sd = mean(se))
