library(randomForest)

grid_search = mod_search$args$rf
# find model with lowest out of sample rmse
grid_search$rmse <- 0
nmods <- nrow(grid_search)

#Everything gets stored to a single tagged list.
if(no_covars){
  load(paste0("output/output_st_",stage,".rdata"))
}else{
  load(paste0("output/output_",stage,".rdata"))
}

rmse <- 10000

for(i in 1:nmods){
  # i <- 1
  train = dplyr::filter(df, yr < (mod_search$args$rf$test_years[i] - mod_search$args$rf$n_years_ahead[i] + 1))  
  test = dplyr::filter(df, yr == mod_search$args$rf$test_years[i]) 
  
  #if this is a projection, get the best model from the exploratory loop
  if(project){
    print(paste(stage,"rf projection year", mod_search$args$rf$test_years[i], " for ", 
                mod_search$args$rf$n_years_ahead[i], " years ahead."))
    mod_frm <- output$exploratory$rf$best_mod #From saved exploration file
    
    #Fit the data
    fit = randomForest(mod_frm, 
                       mtry = output$exploratory$rf$best_mtry,
                       ntree = output$exploratory$rf$best_ntree,
                       data=train)
    pred <- predict(fit, test)
    grid_search$rmse[i] = sqrt(mean((pred-test$dens)^2))
    
  } else{ #just do the exploration
    print(paste(stage,"rf exploration", i, " of ", nmods))
    mod_frm <- mod_search$form$rf[[mod_search$args$rf$mod[i]]]
    mtry <- mod_search$args$rf$mtry[i]
    ntree <- mod_search$args$rf$ntree[i]
    if(mtry<=length(attr(terms(mod_search$form$rf[[1]]),"term.labels"))){
      fit = randomForest(mod_frm, 
                         mtry = mtry,
                         ntree = ntree,
                         data=train)
      grid_search$rmse[i] = sqrt(mean((fit$predicted-train$dens)^2))
      if(grid_search$rmse[i]<rmse){
        rmse <- grid_search$rmse[i]
        best_fit <- fit
      } #This way you don't have to refit the model when saving the best fit
    }else{
      grid_search$rmse[i] <- 1e6
      print(paste("mtry =",mtry,
            "> number of variables = ",
            length(attr(terms(mod_search$form$rf[[1]]),"term.labels"))))
      }
  }

  
  print(paste(stage, "rf rmse",
              round(grid_search$rmse[i])))
}


if(project){
  indx = NA
  best_mod_frm <- NA
  best_mtry <- NA
  best_ntree <- NA
  fit = fit
  
  #re-fit best model
  tmp_output <- list(best_mtry = best_mtry,
                    best_ntree = best_ntree,
                    best_mod = best_mod_frm,
                    train_data = train,
                    best_fit = NA, #too much memory to save the best fit model
                    grid_search = grid_search)
  output$project$rf <- tmp_output
  save(output, file = paste0("output/output_",stage,".rdata"))
}else{
  indx = which.min(grid_search$rmse)
  best_mod_frm <- mod_search$form$rf[[grid_search$mod[indx]]] #Get the model index, not the grid index
  best_mtry <- mod_search$args$rf$mtry[indx]
  best_ntree <- mod_search$args$rf$ntree[indx]
  # fit = randomForest(best_mod_frm,
  #                    mtry = best_mtry,
  #                    ntree = best_ntree,
  #                    data=train)
  #re-fit best model
  tmp_output <- list(best_mtry = best_mtry,
                    best_ntree = best_ntree,
                    best_mod = best_mod_frm,
                    train_data = train,
                    best_fit = NA, #too much memory to save the best fit model 
                    grid_search = grid_search)
  output$exploratory$rf <- tmp_output
  if(save_output){
    if(no_covars){
      save(output, file = paste0("output/output_st_",stage,".rdata"))
    }else{
      save(output, file = paste0("output/output_",stage,".rdata"))
    }
  }
}

