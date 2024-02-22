#use "AllPops.rds"
# out <- output$project$sdm$grid_search
all_pops <- read.csv(paste0('C:/noaa/LARGE_Data/1DataExportJuv_2023_04_18.csv'),
                             header=TRUE,
                             dec=".",
                             stringsAsFactors = FALSE)

out <- readRDS("Spwn_All_Pops.rds")
out$n_years_ahead <- paste(out$n_years_ahead, "years forecasted ahead")
out$survey_pop_type <- unlist(lapply(out$survey_pop_type, function(x) return(paste(x, collapse = ","))))
out$survey_GRTS_type <- unlist(lapply(out$survey_GRTS_type,function(x)return(paste(x,collapse = ","))))
out$survey_GRTS_type[out$survey_GRTS_type=="annual,annua"] <- "Annual"
out$survey_GRTS_type[out$survey_GRTS_type=="annual,annua,three"] <- "Annual + Three"
out$survey_GRTS_type[out$survey_GRTS_type=="annua,annual,Index,nine,once,Supplemental,three"] <- "All GRTS"
out$survey_GRTS_type <- factor(out$survey_GRTS_type, levels = c("All GRTS", "Annual + Three", "Annual"))

# out$survey_pop_type[out$survey_pop_type!="no groups"] <- "Kara pops"
# out$survey_pop_type[out$survey_pop_type=="no groups"] <- "All pops"
out_df <- out %>%
group_by(n_years_ahead, survey_GRTS_type, survey_pop_type) %>%
summarise(mean = mean(rmse))

out_df$survey_pop_type <- factor(out_df$survey_pop_type, levels = out_df$survey_pop_type[out_df$survey_GRTS_type=="All GRTS"][order(-out_df$mean[out_df$survey_GRTS_type=="All GRTS"])])
out_df <- out_df[out_df$survey_pop_type%in%unique(all_pops$PopGrp),]

# Function to calculate ymin for each facet
calculate_ymin <- function(data) {
  ymin <- min(data$mean) - 1
  return(ymin)
}

g <- ggplot(out_df, aes(y = mean, x = survey_pop_type)) +
geom_col(aes(fill = survey_pop_type), position = "dodge") +
facet_wrap(~survey_GRTS_type, ncol = 1, 
           scales = 'free_y', 
           drop = TRUE) +
  # ylim(ymin = calculate_ymin(out_df), ymax = NA) +  # Set y-limits dynamically for each facet
  coord_cartesian(ylim=c(min(out_df$mean),NA)) +
  ylab("Mean RMSE (2017 to 2021)") +
  xlab("Survey design") + 
  theme_bw() +
  geom_blank()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = -0.)) # Set y-axis limits
print(g)
ggsave(g, file = "Spwn_All_pops_RMSE.png", width = 8, height = 10, units = "in")
