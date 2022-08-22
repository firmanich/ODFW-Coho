library(dplyr)
library(sdmTMB)
root <- getwd()
source('code/wrangle_data.r')
source("code/model_exploration.r")
source('code/wrapper_mod_args.r')

#Just do spatiotemporal modeling without any covariates
no_covars <- FALSE
save_output <- FALSE

myStages <- c('Spwn')#rear or Spwn
for(stage in myStages){
  
  #Grab the data for stage
  df <- wrangle_data(stage=stage)
  
  # #run through the models 
  for(project in c(FALSE)){ #Projections vs. model exploration
    # project <- proj
    if(project){
      #Number of years to project ahead
      n_years_ahead = c(0,1,2)# can be 0, 1, 2
      # use avg predictions for last 5 years to train model
      n_test = 5
    }else{ #exploration of different models
      # use last year for test model
      n_years_ahead = 0# Use all of the data
      # only look at the last year of prediction in exploration model
      n_test = 1
    }
    test_years = seq(max(df$yr)-n_test+1, max(df$yr))
  
    #Run through models
    for(mod in c('rf')){
      #Build all of the different models
      #get the old output
      
      model_args(n_years_ahead = n_years_ahead,
                 test_years = test_years)
      
      model_exploration(stage=stage, #Stage
                        mod=mod, #Model 
                        no_covars = no_covars, #deprecated
                        project = project) #is this a projection
    }#end mod
  }#end projection type
}#End stages
