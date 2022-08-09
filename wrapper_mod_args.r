# The dropped variables
#Barriers, mean annual sediment, max gad D, class rank, 

#Stream order, stream slope, 


#Marginal effects,
#covariate all at zero, 

#sdmTMB
#slow convergence, penalized complexity priors - pc priors, you can turn these on to speed up the estimation
#gams in sdmTMB, we could try 

mod_search <- list(years=test_years, 
                   mods = list(rf = list(), gam=ls(), sdm=list()), 
                   args = list(rf=list(), gam=list(), sdm = list()))

rf_forms <- list(
                  m1 = formula(gsub("[\r\n\t]", "","dens ~ yr +
                                       UTM_E_km + 
                                       UTM_N_km"))
            ,m2 = formula(gsub("[\r\n\t]", "","dens ~
                  StrmSlope +
                  WidthM +
                  OUT_DIST +
                  SolMean +
                  MWMT_Index +
                  W3Dppt +
                  StrmPow +
                  SprPpt +
                  IP_COHO +
                  yr"))
            ,m3 = formula(gsub("[\r\n\t]", "","dens ~
                  StrmSlope +
                  WidthM +
                  OUT_DIST +
                  SolMean +
                  MWMT_Index +
                  W3Dppt +
                  StrmPow +
                  SprPpt +
                  IP_COHO +
                  UTM_E_km +
                  UTM_N_km")) #Rf interactions are implicit
            ,m4 = formula(gsub("[\r\n\t]", "","dens ~
                  StrmSlope +
                  WidthM +
                  OUT_DIST +
                  SolMean +
                  MWMT_Index +
                  W3Dppt +
                  StrmPow +
                  SprPpt +
                  IP_COHO +
                  yr +
                  UTM_E_km +
                  UTM_N_km")) #Rf interactions are implicit
            ) #Rf interactions are implicit

if(no_covars){
  rf_forms <- rf_forms[1]
}
mod_search$form$rf <- rf_forms

#create grid of models
if(project){
  rf_args <- expand.grid(test_years = test_years,
                         n_years_ahead = n_years_ahead,
                         mod = 1, 
                         mtry = output$exploratory$rf$best_mtry, 
                         ntree = output$exploratory$rf$best_ntree)
}else{
  rf_args <- expand.grid(test_years = test_years,
                         n_years_ahead = n_years_ahead,
                         mod = 1:length(rf_forms), 
                         mtry = seq(3,15,2), #,15,2 
                         ntree = seq(200,1000,100)) # ,1000,100
  if(no_covars){
    rf_args <- expand.grid(test_years = test_years,
                           n_years_ahead = n_years_ahead,
                           mod = 1:1, 
                           mtry = seq(3,15,2), #,15,2 
                           ntree = seq(200,1000,100)) # ,1000,100
  }
}
mod_search$args$rf <- rf_args

#Gam models
gam_forms <-   list(
                  m1 = formula(gsub("[\r\n\t]", "","dens ~
                                       s(UTM_E_km,UTM_N_km, yr)"))
                  ,m2 = formula(gsub("[\r\n\t]", "","dens ~
                                      s(UTM_E_km,UTM_N_km)"))
                  ,m3 = formula(gsub("[\r\n\t]", "","dens ~
                                       fSTRM_ORDER +
                                       s(StrmSlope,k=4) +
                                       s(WidthM,k=4) +
                                       s(OUT_DIST,k=4) +
                                       s(SolMean,k=4) +
                                       s(MWMT_Index,k=4) +
                                       s(W3Dppt,k=4) +
                                       s(StrmPow,k=4) +
                                       s(SprPpt,k=4) +
                                       s(IP_COHO,k=4)"))
                   ,m4 = formula(gsub("[\r\n\t]", "","dens ~
                                       s(yr) +
                                       fSTRM_ORDER +
                                       s(StrmSlope,k=4) +
                                       s(WidthM,k=4) +
                                       s(OUT_DIST,k=4) +
                                       s(SolMean,k=4) +
                                       s(MWMT_Index,k=4) +
                                       s(W3Dppt,k=4) +
                                       s(StrmPow,k=4) +
                                       s(SprPpt,k=4) +
                                       s(IP_COHO,k=4)"))
                    ,m5 = formula(gsub("[\r\n\t]", "","dens ~
                                       fSTRM_ORDER +
                                       s(StrmSlope,k=4) +
                                       s(WidthM,k=4) +
                                       s(OUT_DIST,k=4) +
                                       s(SolMean,k=4) +
                                       s(MWMT_Index,k=4) +
                                       s(W3Dppt,k=4) +
                                       s(StrmPow,k=4) +
                                       s(SprPpt,k=4) +
                                       s(IP_COHO,k=4) +
                                       s(UTM_E_km,UTM_N_km, yr)"))
                  )#,  #This is NOT the same as the sdmTMB spatiotemporal. THis is a spline in three directions
                    #RMSE for m5 is 441, m6 is 440, but m6 takes about 45 minutes to converge
                    # m6 = formula(gsub("[\r\n\t]", "","dens ~
                    #                    fSTRM_ORDER +
                    #                    s(StrmSlope,k=4) +
                    #                    s(WidthM,k=4) +
                    #                    s(OUT_DIST,k=4) +
                    #                    s(SolMean,k=4) +
                    #                    s(MWMT_Index,k=4) +
                    #                    s(W3Dppt,k=4) +
                    #                    s(StrmPow,k=4) +
                    #                    s(SprPpt,k=4) +
                    #                    s(IP_COHO,k=4) +
                    #                    s(UTM_E,UTM_N, by = as.factor(yr))"))
# ) #This is the same as the sdmTMB spatiotemporal. This is a spline in two direction with slices over time.

if(no_covars){
  gam_forms <- gam_forms[1]
}

mod_search$form$gam <- gam_forms


#create grid of models
if(project){
  gam_args <- expand.grid(test_years = test_years,
                          n_years_ahead = n_years_ahead,
                          mod = 1)
}else{
  gam_args <- expand.grid(test_years = test_years,
                          n_years_ahead = n_years_ahead,
                          mod = 1:length(mod_search$form$gam))
}
mod_search$args$gam <- gam_args

#sdm search
sdm_forms <- list(
                m1 = formula(gsub("[\r\n\t]", "","dens ~ 1
                      "))
                ,m2 = formula(gsub("[\r\n\t]", "","dens ~ 1 +
                       fSTRM_ORDER +
                       StrmSlope +
                       WidthM +
                       OUT_DIST +
                       SolMean +
                       MWMT_Index +
                       W3Dppt +
                       StrmPow +
                       SprPpt +
                       IP_COHO
                      "))
                  )
if(no_covars){
  sdm_forms <- sdm_forms[1]
}
mod_search$form$sdm <- sdm_forms

#create grid of models
if(project){
  sdm_args <- expand.grid(test_years = test_years,
                          n_years_ahead = n_years_ahead,
                          mod = 1, 
                          sp = output$exploratory$sdm$best_sp, 
                          st = output$exploratory$sdm$best_st)
}else{
  sdm_args <- expand.grid(test_years = test_years,
                          n_years_ahead = n_years_ahead,
                          mod = 1:length(sdm_forms), 
                          sp = c(TRUE,FALSE), 
                          st = c("iid",FALSE)
  )
  if(no_covars){
    sdm_args <- expand.grid(test_years = test_years,
                            n_years_ahead = n_years_ahead,
                            mod = 1:1, 
                            sp = c(TRUE), 
                            st = c("iid"))
  }
}

mod_search$args$sdm <- sdm_args

