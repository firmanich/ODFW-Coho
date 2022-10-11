
function_model_exploration <- function(stage = stage,
                              mod = mod, 
                              no_covars = no_covars, 
                              project = project,
                              output = output,
                              mod_search = mod_search,
                              save_output = FALSE){
  

  #@stage "rear" or "Spwn" stage
  #@mod rf, gam, sdm 
  #@
  #@output is a saved tagged list of both exploratory and projected output
  #Grab the model search arguments from the saved tagged list
  
  search <- mod_search$args[[mod]]

  # find model with lowest out of sample rmse
  search$rmse <- 1e6
  search$AIC <- 1e6

  #The number of model forms
  nforms <- nrow(search)

  #Reset the model selection criteria
  bestRMSE <- 1e6
  bestAIC <- 1e6

  print(paste("saving output", save_output))
  for(i in 1:nforms){
    train <- dplyr::filter(df,
                          yr < (mod_search$args[[mod]]$test_years[i] - mod_search$args[[mod]]$n_years_ahead[i] + 1)) %>%
      dplyr::mutate(fYr = as.factor(yr))

    test <- dplyr::filter(df, yr == mod_search$args[[mod]]$test_years[i]) %>%
      dplyr::mutate(fYr = as.factor(yr))


    #just use the same mesh for all sdm projections and explorations
    if(mod == 'sdm'){
      #For the sdmTMb package you have to predict over all years.
      #This is a little different than the gam and rf packages
      test = dplyr::filter(df, yr <= search$test_years[i]) %>%
        mutate(fYr = as.factor(yr))

      #Create the mesh
      mesh <- make_mesh(train, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
    }

    if(project){
      print(paste(stage, mod, "projection year", search$test_years[i], " for ",
                  mod_search$args[[mod]]$n_years_ahead[i], " years ahead."))

      best_mod_frm <- output$exploratory[[mod]]$best_mod #From saved exploration file

      if(mod=="sdm"){
        fit <- sdmTMB(best_mod_frm,
                      data = train,
                      mesh = mesh,
                      family = tweedie(link = "log"),
                      time = "yr",
                      spatial = output$exploratory[[mod]]$best_sp,
                      spatiotemporal = output$exploratory[[mod]]$best_st,
                      anisotropy = TRUE,
                      extra_time = unique(test$yr[test$yr>max(train$yr)]), #Why is this necessary if the years are the same?
                      silent=TRUE)

        #Model prediction
        pred <- predict(fit,
                        test,
                        re_form_iid = NA)


        search$rmse[i] = sqrt(mean((test$dens[test$yr==mod_search$args[[mod]]$test_years[i]] -
                                           exp(pred$est[test$yr==mod_search$args[[mod]]$test_years[i]]))^2))

        search$AIC[i] = AIC(fit)
      }

      if(mod=="rf"){
        #Fit the data
        fit = randomForest(best_mod_frm,
                           mtry = output$exploratory[[mod]]$best_mtry,
                           ntree = output$exploratory[[mod]]$best_ntree,
                           data=train)

        pred <- predict(fit, test)
        search$rmse[i] = sqrt(mean((pred-test$dens)^2))
      }

      if(mod=='gam'){
        #Refit the best model with the training data. You have to refit to each new training data set
        fit <-  gam(best_mod_frm, data=train, family = "tw")
        #Predict the year in questions
        p <-  predict(fit,test, family = "tw")
        #Save it to the projection grid search
        search$rmse[i] <- sqrt(mean((exp(p)-test$dens)^2))
        search$AIC[i] <- AIC(fit)
      }
    }#project = TRUE

    if(!project){#just do the exploration
      print(paste(stage,mod," exploration", i, " of ", nforms))
      mod_frm <- mod_search$form[[mod]][[search$mod[[i]]]]

      if(mod=="rf"){
        #Fit the data
        if(mod_search$args[[mod]]$mtry[i]<=length(attr(terms(mod_frm),"term.labels"))){
          fit = randomForest(mod_frm,
                           mtry = mod_search$args[[mod]]$mtry[i],
                           ntree = mod_search$args[[mod]]$ntree[i],
                           data=train)

          pred <- predict(fit, test)
          search$rmse[i] = sqrt(mean((pred-test$dens)^2))
        }else{
          search$rmse[i] <- 1e6
        }
      }

      if(mod=="sdm"){
        fit <- sdmTMB(mod_frm,
                    data = train,
                    mesh = mesh,
                    family = tweedie(link = "log"),
                    time = "yr",
                    spatial = mod_search$args[[mod]]$sp[i],
                    spatiotemporal = mod_search$args[[mod]]$st[i],
                    anisotropy = TRUE,
                    extra_time = unique(test$yr[test$yr>max(train$yr)]), #Why is this necessary if the years are the same?
                    silent=TRUE)
        pred <- predict(fit, train, re_form_iid = NA)
        #Save the root mean square error in the search
        search$rmse[i] = sqrt(mean((train$dens - exp(pred$est))^2))
        search$AIC[i] <- AIC(fit)
      }

      if(mod=="gam"){
          #Get the model forms from the "wrapper_mod_args.r script")
        # mod_search$args[[mod]]$mod[i] <- i
        # mod_search$args[[mod]]$args[i] <- NA
        fit <-  gam(mod_frm, data=train, family = "tw")
        search$rmse[i] <- sqrt(mean((fit$fitted.values-train$dens)^2))
        search$AIC[i] <- AIC(fit)
      }

      if((search$AIC[i]<bestAIC) & (mod=='sdm' | mod=='gam')){
        bestAIC <- search$AIC[i]
        best_fit <- fit
      } #This way you don't have to refit the model when saving the best fit
      if((search$rmse[i]<bestRMSE) & (mod=='rf')){
        bestRMSE <- search$rmse[i]
        best_fit <- fit
      } #This way you don't have to refit the model when saving the best fit
    }


    print(paste(stage,mod,"rmse",
                round(search$rmse[i],3)))
  }#end model forms

  #Update the output for each model iteration
  if(project){
    #re-fit best model
    if(mod=='rf'){
      output$project$rf <- list(best_mtry = output$exploratory$rf$best_mtry,
                                 best_ntree = output$exploratory$rf$best_ntree,
                                 best_mod = output$exploratory$rf$best_mod,
                                 # best_fit = output$exploratory$rf$best_fit,
                                 grid_search = search)
    }
    if(mod=='gam'){
      output$project$sdm <- list(best_mod = output$exploratory$gam$best_mod,
                                 best_fit = output$exploratory$gam$best_fit,
                                 grid_search = search)
    }
    if(mod=='sdm'){
      output$project$sdm <- list(best_st = output$exploratory$sdm$best_st,
                                 best_sp = output$exploratory$sdm$best_sp,
                                 best_mod = output$exploratory$sdm$best_mod,
                                 best_fit = output$exploratory$sdm$best_fit,
                                 grid_search = search)
    }
    if(save_output){
      print("saving output")
      print(names(output))
      save(output, file = paste0("output/output_",stage,".rdata"))
    }
  }else{
    if(mod=='sdm'){
      indx = which.min(search$AIC)
      best_mod_frm <- mod_search$form[[mod]][[search$mod[indx]]] #Get the model index, not the grid index
      best_sp <- search$sp[indx]
      best_st <- search$st[indx]
      #re-fit best model

      output$exploratory[[mod]] <- append(output$exploratory[[mod]],
                                       list(best_st = best_st,
                                            best_sp = best_sp,
                                            best_mod = best_mod_frm,
                                            best_fit = best_fit,
                                            grid_search = search))
    }
    if(mod=='gam'){
      indx = which.min(search$AIC)
      best_mod_frm <- mod_search$form[[mod]][[search$mod[indx]]] #Get the model index, not the grid search index

      output$exploratory[[mod]] <- append(output$exploratory[[mod]],
                                          list(best_mod = best_mod_frm,
                                               best_fit = best_fit,
                                               grid_search = search))
    }

    if(mod=='rf'){
      indx = which.min(search$rmse)
      best_mod_frm <- mod_search$form[[mod]][[search$mod[indx]]] #Get the model index, not the grid index
      best_mtry <- mod_search$args$rf$mtry[indx]
      best_ntree <- mod_search$args$rf$ntree[indx]

      output$exploratory[[mod]] <- append(output$exploratory[[mod]],
                                          list(best_mtry = best_mtry,
                                               best_ntree = best_ntree,
                                               best_mod = best_mod_frm,
                                               train_data = train,
                                               best_fit = NA, #too much memory to save the best fit model
                                               grid_search = search))
    }

    if(save_output){
      save(output, file = paste0("output/output_",stage,".rdata"))
    }
  }
  return(output)
}



