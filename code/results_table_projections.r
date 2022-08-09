library(ggplot2)
library(dplyr)
#Run through models
load("C:/noaa/projects/ODFW-Coho/output/output.rData")
gam <- (output$project$gam$grid_search) %>%  
  mutate(mod = "gam") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

sdm <- (output$project$sdm$grid_search) %>%  
  mutate(mod = "sdm") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

rf <- output$project$rf$grid_search %>%  
  mutate(mod = "rf") %>% 
  dplyr::select(c(mod,n_years_ahead,rmse))

rmse_mean <- bind_rows(gam,sdm,rf) %>% 
  group_by(mod,n_years_ahead) %>%
  summarize(rmse = mean(rmse, na.rm = TRUE))

g <- ggplot(aes(y = rmse, x = mod, fill = as.factor(n_years_ahead)), 
            data = rmse_mean) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Five-year mean RMSE") +
  xlab(" Model ") +
  guides(fill=guide_legend(title="Number of \nyears ahead"))

print(g)
