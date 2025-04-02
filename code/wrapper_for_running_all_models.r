# rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search.r")
source("./code/function_run_models.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)

# Stage <- 'rear' #rear or Spwn
for(Stage in c('rear')){
  if(Stage=='Spwn'){
    file <- "spawn_dataHR17v4"
  }else{
    file <- "rear_dataHR17v5"
  }
  
  # study_pops <- c('Nehalem',
  #                 'Alsea',
  #                 'Coos',
  #                 'Lower Umpqua')
  # 
  # #This is counter intuitive, but these are the populations left out of the analysis
  # pops_in_core <- read.csv(paste0('C:/noaa/LARGE_Data/',file,'.csv'),
  #                          header=TRUE,
  #                          dec=".",
  #                          stringsAsFactors = FALSE) %>%
  #   filter(!PopGrp %in% study_pops) %>%
  #   distinct(PopGrp)
  # 
  # # # 
  # pops_not_in_core <- read.csv(paste0('C:/noaa/LARGE_Data/',file,'.csv'),
  #                              header=TRUE,
  #                              dec=".",
  #                              stringsAsFactors = FALSE) %>%
  #   filter(PopGrp %in% study_pops) %>%
  #   distinct(PopGrp)
  # 
  # # 
  # all_pops <- read.csv(paste0('C:/noaa/LARGE_Data/',file,'.csv'),
  #                      header=TRUE,
  #                      dec=".",
  #                      stringsAsFactors = FALSE) %>%
  #   filter(Stratum!="Lakes") %>% 
  #   filter(!(PopGrp%in%c("MSDependent","NCDependent","MCDependent"))) %>% 
  #   group_by(PopGrp) %>%
  #   droplevels() %>% 
  #   summarise(n = n())
  # 
  # pops_in_core <- read.csv(paste0('C:/noaa/LARGE_Data/',file,'.csv'),
  #                          header=TRUE,
  #                          dec=".",
  #                          stringsAsFactors = FALSE) %>%
  #   filter(!PopGrp %in% study_pops) %>%
  #   distinct(PopGrp)
  
  # survey_pop_list <- lapply(as.matrix(all_pops$PopGrp), function(x) as.list(x))

    
  survey_pop_list <- list("allGrps" = c("no groups"))

  survey_GRTS_list <- list(
    # c("Index")
    c("annual","annua")
    ,c("annual","annua","three")
    ,c("annua","annual","Index","nine","once","Supplemental","three")
  )
  
  for(maxYr in c(2021)){
    for(proj in c(TRUE)){
      for(mm in c('gam')){ #model
        for(ss in Stage){ #life stage
          for(si in c('spatial')){ #testing models based on survey design or temporal forecasting, **** Both can be run from 'spatial' by looping over proj <- c(TRUE,FALSE)
            
            if(si =="temporal"){ #this scenario of n_years-ahead <- 0 and survey_projection <- FALSE determines the best fit model given all of the data
              n_years_ahead <- c(0,1,2)
              survey_projection <- FALSE #Keep this set to false for Temporal analysis
            }
            if(si=="spatial"){ #Given the best fit model from above, now ask questions about temporal and spatial projects.
              n_years_ahead <- c(0,1,2)
              survey_projection <- proj
            }
            
            #load the saved output
            cat('\n\n')
            if(si=="pop"){
              # file <- paste0("output/temporal_output_",ss,"_",maxYr,"_2.rdata")
            }
            if(si!="pop"){
              file <- paste0("output/",si,"_output_",ss,"_",maxYr,".rdata")
            }
            load(file = file)
            #update the new output
            output <- function_run_models(project = proj, #This is whether you want to project into the future. When set to FALSE, n_years_ahead is fixed to zero
                                          survey_projection = survey_projection, #years into the future to project.
                                          survey_GRTS_type = survey_GRTS_list, #which grits design. This is for non-projections
                                          survey_pop_type = survey_pop_list, #which populations to project
                                          no_covars = FALSE, #Deprecated   - whether a covariate only model
                                          save_output = TRUE, #Do you want to save and over-write the output
                                          stage = ss, #Which stage rear or Spwn
                                          mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                          maxYr = maxYr,
                                          survey_pop = survey_pop_list,
                                          n_years_ahead = n_years_ahead, #predictions into the future, reduces the number of years in the training data set
                                          n_test = 5) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019
            #save the updated output
            save(output, file = file)
          }
        }
      }
    }
  }
}
