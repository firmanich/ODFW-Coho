function_run_models <- function(project = TRUE, #This is whether you want to project into the future 
                                no_covars=FALSE, #Deprecated   - whether a covariate only model
                                save_output=TRUE, #Do you want to save the output
                                stage = 'rear', #Which stage rear or Spwn
                                mod = c('rf'), # the type of model 'rf', 'gam', 'glm'
                                n_years_ahead = c(0,1,2), #predictions into the future
                                n_test = 5){ #Number of years in the RMSE model comparison, if it's 5 the you're comparing 2015 through 2019
  #Katie and I are working on the doc.
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
