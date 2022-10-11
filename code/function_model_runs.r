function_run_models <- function(project = TRUE,
                                no_covars=FALSE,
                                save_output=TRUE,
                                stage = 'rear',
                                mod = c('rf'),
                                n_years_ahead = c(0,1,2),
                                n_test = 5){
  library(dplyr)
  library(tidyr)
  library(sdmTMB)
  library(randomForest)
  library(mgcv)
  
  #Load the dependent functions
  root <- getwd()
  
  #load output
  # stage <- "rear"
  load(file = paste0("output/output_",stage,".rdata"))

  #Grab the data for stage
  df <- function_wrangle_data(stage=stage,
                              dir = root)

  #These are the tested years, as opposed to the training years
  # n_test <- 5
  test_years = seq(max(df$yr)-n_test+1, max(df$yr))

  #tagged list of model arguments and models to search over
  # n_years_ahead <- 0
  # project <- TRUE
  # no_covars <- TRUE
  mod_search <- function_model_search(test_years = test_years,
                                      n_years_ahead = n_years_ahead,
                                      project = project,
                                      no_covars = no_covars,
                                      output = output)

  # print(mod_search)
  #Output of model exploration
  # mod <- "sdm"
  # save_output <- TRUE
  # i <- 1
  function_model_exploration(stage=stage, #Stage
                               mod=mod, #Model
                               no_covars = no_covars, #deprecated
                               project = project,
                               mod_search = mod_search,
                               save_output = save_output,
                               output = output) #is this a projection
  
  #return output from exploration or projections
  # ifelse(project,
  #        pe_idx <- "project",
  #        pe_idx <- "exploratory")
  # return(tmp_output[[pe_idx]][[mod]])
  
}#End stages
