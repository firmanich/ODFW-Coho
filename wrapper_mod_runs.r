library(dplyr)
library(sdmTMB)
source('wrangle_data.r')
root <- getwd()

#Just do spatiotemporal modeling without any covariates
no_covars <- FALSE
save_output <- TRUE

myStages <- c('Spwn')#rear or Spwn
for(stage in myStages){
  
  df <- wrangle_data(stage=stage)
  # #run through the models 
  for(project in c(FALSE)){ #Projections vs. model exploration
    # project <- proj
    if(project){
      #Number of years to project ahead
      n_years_ahead = c(0,1,2)# can be 0, 1, 2
      # use avg predictions for last 5 years to train model
      n_test = 5
    }else{
      # use last year for test model
      n_years_ahead = 0# Use all of the data
      # only look at the last year of prediction in exploration model
      n_test = 1
    }
    test_years = seq(max(df$yr)-n_test+1, max(df$yr))
  
  
    #Run through models
    for(mod in c('sdm')){
      #Build all of the different models
      #get the old output
      source("wrapper_mod_args.r")
      source(paste0("C:/noaa/projects/ODFW-coho/code/exploratory_",mod,"_chasco_comb.r"))
    }#end mod
  }#end projection type
}#End stages
