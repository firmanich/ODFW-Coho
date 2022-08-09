library(mgcv)

grid_search = mod_search$args$gam
# find model with lowest out of sample rmse
grid_search$rmse <- 0
grid_search$AIC <- 0
nmods <- nrow(grid_search)

#Everything gets stored to a single tagged list.
#Everything gets stored to a single tagged list.
if(no_covars){
  load(paste0("output/output_st_",stage,".rdata"))
}else{
  load(paste0("output/output_",stage,".rdata"))
}

for(i in 1:nmods){
  # i <- 1
  train = dplyr::filter(df, yr < (mod_search$args$gam$test_years[i] - mod_search$args$gam$n_years_ahead[i] + 1))  
  test = dplyr::filter(df, yr == mod_search$args$gam$test_years[i]) 
  
  #if this is a projection, get the best model from the exploratory loop
  if(project){
    print(paste(stage, "gamm projection year", 
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
    grid_search$rmse[i] <- sqrt(mean((exp(p)-test$dens)^2))
    grid_search$AIC[i] <- AIC(fit)
    
  } else{ #just do the exploration
    print(paste(stage,"gam exploration", i, " of ", nmods))
    
    #Get the model forms from the "wrapper_mod_args.r script")
    mod_frm <- mod_search$form$gam[[mod_search$args$gam$mod[i]]]
    grid_search$mod[i] <- i
    grid_search$args[i] <- NA
    fit <-  gam(mod_frm, data=train, family = "tw")
    print(paste(stage,"gam", AIC(fit)))
    grid_search$rmse[i] <- sqrt(mean((fit$fitted.values-train$dens)^2))
    grid_search$AIC[i] <- AIC(fit)
  }
  
  print(paste(stage, "rmse",
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
  save(output, file = paste0("output/output_",stage,".rdata"))
}else{
  indx = which.min(grid_search$AIC)
  best_mod_frm <- mod_search$form$gam[[grid_search$mod[indx]]] #Get the model index, not the grid search index

  #re-fit best model
  fit <-  gam(best_mod_frm, data=train, family = "tw")
  
  tmp_output <- list(best_mod = best_mod_frm,
                    best_fit = fit,
                    grid_search = grid_search)
  output$exploratory$gam <- tmp_output
  if(save_output){
    if(no_covars){
      save(output, file = paste0("output/output_st_",stage,".rdata"))
    }else{
      save(output, file = paste0("output/output_",stage,".rdata"))
    }
  }
}
