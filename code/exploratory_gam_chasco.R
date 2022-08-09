library(mgcv)

grid_search = mod_search$args$gam
# find model with lowest out of sample rmse
grid_search$rmse <- 0
nmods <- nrow(grid_search)

#Everything gets stored to a single tagged list.
#Everything gets stored to a single tagged list.
if(no_covars){
  load("output/output_st.rdata")
}else{
  load("output/output.rdata")
}

for(i in 1:nmods){
  # i <- 1
  train = dplyr::filter(df, JuvYr < (mod_search$args$gam$test_years[i] - mod_search$args$gam$n_years_ahead[i] + 1))  
  test = dplyr::filter(df, JuvYr == mod_search$args$gam$test_years[i]) 
  
  #if this is a projection, get the best model from the exploratory loop
  if(project){
    print(paste("Projection year", 
                mod_search$args$gam$test_years[i], "for ", 
                mod_search$args$gam$n_years_ahead[i], " years ahead."))
    best_mod_frm <- output$exploratory$gam$best_mod  #From saved exploration file
    grid_search$mod[i] <- NA
    grid_search$args[i] <- NA
    
    #Refit the best model with the training data. You have to refit to each new training data set
    fit <-  gam(best_mod_frm, data=train, family = "tw")
    #Predict the year in questions
    p <-  predict(fit,test, family = "tw")
    #Save it to the projection grid search
    grid_search$rmse[i] <- sqrt(mean((exp(p)-test$Juv.km)^2))
    
  } else{ #just do the exploration
    print(paste("gam exploration", i, " of ", nmods))
    #Get the model forms from the "wrapper_mod_searches.r script")
    mod_frm <- mod_search$form$gam[[mod_search$args$gam$mod[i]]]
    grid_search$mod[i] <- i
    grid_search$args[i] <- NA
    fit <-  gam(mod_frm, data=train, family = "tw")
    grid_search$rmse[i] <- sqrt(mean((fit$fitted.values-train$Juv.km)^2))
  }
  
  print(paste("rmse",
              round(grid_search$rmse[i])))
  
}


#Store output  
# output <- list(project = list(rf=list(), gam = list(), sdm = list()),
#                exploratory = list(rf=list(), gam = list(), sdm = list()))

if(project){
  indx = NA
  best_mod_frm <- NA
  fit = fit

  #re-fit best model
  tmp_output <- list(best_mod = mod_frm,
                    best_fit = fit,
                    grid_search = grid_search)
  output$project$gam <- tmp_output
  save(output, file = "output/output.rdata")
}else{
  indx = which.min(grid_search$rmse)
  best_mod_frm <- mod_search$form$gam[[grid_search$mod[indx]]] #Get the model index, not the grid search index
  fit <-  gam(best_mod_frm, data=train, family = "tw")
  
  #re-fit best model
  tmp_output <- list(best_mod = best_mod_frm,
                    best_fit = fit,
                    grid_search = grid_search)
  output$exploratory$gam <- tmp_output
  if(save_output){
    if(no_covars){
      save(output, file = "output/output_st.rdata")
    }else{
      save(output, file = "output/output.rdata")
    }
  }
}
