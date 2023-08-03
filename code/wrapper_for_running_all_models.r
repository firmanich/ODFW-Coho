rm(list=ls())
source("./code/function_wrangle_data.r")
source("./code/function_model_search2.r")
source("./code/function_run_models.r")
source("./code/function_model_exploration.r")

require(sdmTMB)
library(tidyr)
library(dplyr)
 

for(maxYr in c(2019,2021)){
  for(proj in c(FALSE,TRUE)){
    for(mm in c('rf')){ #model
      for(ss in c('rear','Spwn')){ #life stage
        for(si in c('spatial','temporal')){ #testing models based on survey design or temporal forecasting
          
          if(si=="spatial"){
            n_years_ahead <- c(0)
            survey_projection <- proj
          }else{
            n_years_ahead <- c(0,1,2)
            survey_projection <- FALSE #Keep this set to false for Temporal analysis
          }
          
          #load the saved output
          cat('\n\n')
          file <- paste0("output/",si,"_output_",ss,"_",maxYr,".rdata")
          load(file = file)
          #update the new output
          output <- function_run_models(project = proj, #This is whether you want to project into the future
                                        survey_projection = survey_projection, #years into the future to project.
                                        survey_type = list(
                                          c("Index")
                                          ,c("annual","annua")
                                          ,c("annual","annua","three")
                                          ,
                                          c("annua","annual","Index","nine","once","Supplemental","three")
                                        ),
                                        no_covars = FALSE, #Deprecated   - whether a covariate only model
                                        save_output = TRUE, #Do you want to save and over-write the output
                                        stage = ss, #Which stage rear or Spwn
                                        mod = mm, # the type of model 'rf', 'gam', 'sdm'
                                        maxYr = maxYr,
                                        n_years_ahead = n_years_ahead, #predictions into the future, reduces the number of years in the training data set
                                        n_test = 5) #Number of years in the RMSE model compariso\n, if it's 5 the you're comparing 2015 through 2019
          #save the updated output
          save(output, file = file)
        }
      }
    }
  }
}
