library(ggplot2)
library(dplyr)
#Run through models
load("C:/noaa/projects/ODFW-Coho/output/output_rear.rData")
gam <- (output$project$gam$grid_search) %>%  
  mutate(mod = "GAM (mgcv)") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

sdm <- (output$project$sdm$grid_search) %>%  
  mutate(mod = "GLM (sdmTMB)") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

rf <- output$project$rf$grid_search %>%  
  mutate(mod = "Random forest (randomForest)") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

rmse_mean <- bind_rows(gam,sdm,rf) %>% 
  group_by(mod,n_years_ahead) %>%
  summarize(rmse = mean(rmse, na.rm = TRUE))

g <- ggplot(aes(y = rmse, x = mod, fill = as.factor(n_years_ahead)), 
            data = rmse_mean[rmse_mean$n_years_ahead!=0,]) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Mean RMSE 2015 to 2019") +
  xlab(" Model ") +
  guides(fill=guide_legend(title="Number of \nyears ahead")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim=c(350,380))
  
png("output/ggplot_predictive_rmse_2015_2019.png", height = 400, width = 600)
print(g)
dev.off()
