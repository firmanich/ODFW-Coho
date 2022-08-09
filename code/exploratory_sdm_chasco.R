library(sdmTMB)

grid_search = mod_search$args$sdm
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
rmse <- 10000

for(i in 1:nmods){
  # i <- 1
  train = dplyr::filter(df, JuvYr < (mod_search$args$sdm$test_years[i] - mod_search$args$sdm$n_years_ahead[i] + 1))
  train$fJuvYr <- as.factor(train$JuvYr)

  test <- df[df$JuvYr<=(mod_search$args$sdm$test_years[i]),]
  test$fJuvYr <- as.factor(test$JuvYr)
  
  mesh <- make_mesh(train, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
  #if this is a projection, get the best model from the exploratory loop
  if(project){
    print(paste("Projection year", mod_search$args$sdm$test_years[i], " for ", 
                mod_search$args$sdm$n_years_ahead[i], " years ahead."))
    mod_frm <- output$exploratory$sdm$best_mod #From saved exploration file
    fit <- sdmTMB(mod_frm,
                  data = train,
                  mesh = mesh, 
                  family = tweedie(link = "log"),
                  time = "JuvYr",
                  spatial = grid_search$sp[i],
                  spatiotemporal = grid_search$st[i],
                  anisotropy = TRUE,
                  extra_time = unique(test$JuvYr[test$JuvYr>max(train$JuvYr)]), #Why is this necessary if the years are the same?
                  silent=TRUE)
    pred = predict(fit, test, re_form_iid = NA)
    # est <- exp(pred$est[pred$JuvYr==(mod_search$args$sdm$test_years[i])])
    # obs <- test$Juv.km[test$JuvYr==(mod_search$args$sdm$test_years[i])]
    grid_search$rmse[i] = sqrt(mean((test$Juv.km[test$JuvYr==mod_search$args$sdm$test_years[i]] - 
                                       exp(pred$est[test$JuvYr==mod_search$args$sdm$test_years[i]]))^2))
    
  } else{ #just do the exploration
    print(paste("sdm exploration", i, " of ", nmods))
    mod_frm <- mod_search$form$sdm[[mod_search$args$sdm$mod[i]]]
    fit <- sdmTMB(mod_frm,
                  data = train,
                  mesh = mesh, 
                  family = tweedie(link = "log"),
                  time = "JuvYr",
                  spatial = grid_search$sp[i],
                  spatiotemporal = grid_search$st[i],
                  anisotropy = TRUE,
                  extra_time = unique(test$JuvYr[test$JuvYr>max(train$JuvYr)]), #Why is this necessary if the years are the same?
                  silent=TRUE)
    pred = predict(fit, train, re_form_iid = NA)
    # est <- exp(pred$est[pred$JuvYr==(mod_search$args$sdm$test_years[i])])
    # obs <- test$Juv.km[test$JuvYr==(mod_search$args$sdm$test_years[i])]
    grid_search$rmse[i] = sqrt(mean((train$Juv.km - exp(pred$est))^2))
    
    if(grid_search$rmse[i]<rmse){
      rmse <- grid_search$rmse[i]
      best_fit <- fit
    } #This way you don't have to refit the model when saving the best fit
  }
  

  print(paste("rmse",
              round(grid_search$rmse[i],3)))
}


if(project){
  #re-fit best model
  tmp_output <- list(best_st = output$exploratory$sdm$best_st,
                    best_sp = output$exploratory$sdm$best_sp,
                    best_mod = output$exploratory$sdm$best_mod,
                    best_fit = output$exploratory$sdm$best_fit,
                    grid_search = grid_search)
  output$project$sdm <- tmp_output
  save(output, file = "output/output.rdata")
}else{
  indx = which.min(grid_search$rmse)
  best_mod_frm <- mod_search$form$sdm[[grid_search$mod[indx]]] #Get the model index, not the grid index
  best_sp <- mod_search$args$sdm$sp[indx]
  best_st <- mod_search$args$sdm$st[indx]
  #re-fit best model
  tmp_output <- list(best_st = best_st,
                     best_sp = best_sp,
                     best_mod = best_mod_frm,
                     best_fit = best_fit,
                     grid_search = grid_search)
  output$exploratory$sdm <- tmp_output
  if(save_output){
    if(no_covars){
      save(output, file = "output/output_st.rdata")
    }else{
      save(output, file = "output/output.rdata")
    }
  }
}



