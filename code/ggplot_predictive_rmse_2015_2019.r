library(ggplot2)
library(dplyr)
library(tidyr)
#Run through models
stage <- "rear"
load(paste0("output/output_",stage,".rData"))

gam <- (output$project$gam$grid_search) %>%  
  mutate(mod = "GAMM ") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

sdm <- (output$project$sdm$grid_search) %>%  
  mutate(mod = "GLMM ") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

rf <- output$project$rf$grid_search %>%  
  mutate(mod = "Random forest ") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))


rmse_mean <- dplyr::bind_rows(gam,sdm,rf) %>% 
  dplyr::group_by(mod,n_years_ahead) %>%
  dplyr::summarise(rmse = mean(rmse))

ifelse(stage=="rear", lims <- c(350,380), lims <- c(19,31))

g <- ggplot(aes(y = rmse, x = mod, fill = as.factor(n_years_ahead)), 
            data = rmse_mean[rmse_mean$n_years_ahead!=0,]) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Mean RMSE 2015 to 2019") +
  xlab(" Model ") +
  guides(fill=guide_legend(title="Number of \nyears ahead")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  coord_cartesian(ylim=lims)
  
png("output/ggplot_predictive_rmse_2015_2019.png", height = 400, width = 600)
print(g)
dev.off()
