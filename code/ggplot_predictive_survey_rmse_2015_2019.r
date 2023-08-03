library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)

#Run through models
plotlist <- list()
icnt <- 1
for(i in c('rear','Spwn')){
  stage <- i
  load(paste0("output/spatial_output_rear_2019.rData"))
  
  ifelse(stage=="rear", lims <- c(140,380), lims <- c(10,65))
  

  gam <- (output$project$gam$grid_search)
  for(i in 1:nrow(gam)) gam$survey[i] <- paste(unlist(gam$survey_type[[i]]),collapse="_")
  gam <- gam %>%  
    mutate(model = "GAMM") %>% 
    dplyr::select(c(model,survey,rmse))
  
  sdm <- (output$project$sdm$grid_search)
  for(i in 1:nrow(sdm)) sdm$survey[i] <- paste(unlist(sdm$survey_type[[i]]),collapse="_")
  sdm <- sdm %>%  
    mutate(model = "GLMM") %>% 
    dplyr::select(c(model,survey,rmse))
  
  rf <- (output$project$rf$grid_search)
  for(i in 1:nrow(sdm)) rf$survey[i] <- paste(unlist(rf$survey_type[[i]]),collapse="_")
  rf <- rf %>%  
    mutate(model = "Random\nforest") %>% 
    dplyr::select(c(model,survey,rmse))
  
  
  rmse_mean <- as.data.frame(rbind(gam,sdm,rf))
  rmse_mean$survey <- unlist(rmse_mean$survey)
  
  rmse_mean$survey <- as.factor(rmse_mean$survey)
  levels(rmse_mean$survey) <- c("Complete", "Annual", "Annual & Three", "No Survey")
  # levels(rmse_mean$survey) <- levels(rmse_mean$survey)[c(4,2,3,1)]
  
    # rmse_mean$survey <- factor(rmse_mean$survey, levels = rmse_mean$survey[order(rmse_mean$rmse)])
  # x$name  # notice the changed order of factor levels
#   ifelse(stage=="rear", lims <- c(140,380), lims <- c(8,35))
#   
  plotlist[[icnt]] <- ggplot(aes(y = rmse, x = model, fill = (survey)),
               data = rmse_mean) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_grey()+
    ylab("") +
    xlab("") +
    guides(fill=guide_legend(title="Survey design")) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    coord_cartesian(ylim=lims)
  
  # print(p)
  icnt <- icnt + 1
  
}
# 
gg <- ggpubr::ggarrange(plotlist = plotlist,
                        ncol = 2,
                        legend = 'right',
                        labels = c("A","B"),
                        common.legend = TRUE)
# 
gg <- ggpubr::annotate_figure(gg,
                left = ggpubr::text_grob("Mean RMSE Survery 2015 to 2019", color = "black", rot = 90),
                fig.lab = "", fig.lab.face = "bold")

print(gg)

# ggsave(file = "./output/ggplot_predictive_survey_rmse_2015_2019.png", gg, device = "png", dpi = 300, height = 4, width = 6, units="in")

  # dev.off()

