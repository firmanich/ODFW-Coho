library(dplyr)
library(mgcv)
library(sdmTMB)
library(ggplot2)

#First read in the best models
root <- getwd()

#'MWMT_Index','W3Dppt','SprPpt','IP_COHO', 'SolMean','StrmSlope', 'OUT_DIST','PopGrp',
myVars <- c('Juv.km','UTM_E','UTM_N',
            'WidthM','W3Dppt',
            'MWMT_Index','W3Dppt',
            'SprPpt','IP_COHO', 
            'SolMean','StrmSlope',
            'OUT_DIST','JuvYr')

df <- read.csv("juvData.csv")

#Wrangling
#All models use the same data.
scale_this <- function(x) as.vector(scale(x))
df <- df %>%
  dplyr::select(all_of(myVars)) %>%
  mutate(MWMT_Index= ifelse(is.na(MWMT_Index), mean(MWMT_Index, na.rm=TRUE), MWMT_Index)) %>% 
  mutate(UTM_E_km = UTM_E/1000,
         UTM_N_km = UTM_N/1000,
         fJuvYr = as.factor(JuvYr),
         WidthM = scale_this(WidthM),
         W3Dppt = scale_this(W3Dppt),
         MWMT_Index = scale_this(MWMT_Index),
         SprPpt = scale_this(SprPpt),
         IP_COHO = scale_this(IP_COHO), 
         SolMean = scale_this(SolMean),
         StrmSlope = scale_this(StrmSlope),
         OUT_DIST = scale_this(OUT_DIST)
  )


library(randomForest)

n = c(0,1,2)# can be 0, 1, 2
mods <- c("rf","gam")
n_test <- 1
test_years = seq(max(df$JuvYr)-n_test+1, max(df$JuvYr))

myProj <- function(n_years_ahead,mymod){
  train = dplyr::filter(df, JuvYr < (test_years - n_years_ahead + 1))  
  train$fJuvYr <- as.factor(train$JuvYr)
  # print(train)
  test = dplyr::filter(df, JuvYr <= test_years)
  test$fJuvYr <- as.factor(test$JuvYr)
  
  if(mymod=="rf"){
    load("output/output_rf.rData")
    rf_res <- output$rf
    best_mod_frm <- rf_res$best_mod
    best_mtry <- rf_res$best_mtry
    best_ntree <- rf_res$best_ntree
    fit = randomForest::randomForest(best_mod_frm,
                                     mtry = best_mtry,
                                     ntree = best_ntree,
                                     data=train)
    pred <- predict(fit,test)
  }
  
  if(mymod=="gam"){
    load("output/output_gam.rData")
    gam_res <- output$gam
    best_mod_frm <- gam_res$best_mod
    fit <-  gam(best_mod_frm, data=train, family = "tw")
    #Predict the shape grid that Julie sent.
    pred <- predict(fit, newdata = test, type="response")
  }

  if(mymod=="sdm"){
    load("output/output_sdm.rData")
    sdm_res <- output$sdm
    best_mod_frm <- sdm_res$best_mod
    best_st <- sdm_res$best_st
    best_sp <- sdm_res$best_sp
    mesh <- make_mesh(train, c("UTM_E_km", "UTM_N_km"), cutoff = 10)
    fit <- sdmTMB(best_mod_frm,
                  data = train,
                  mesh = mesh, 
                  family = tweedie(link = "log"),
                  time = "JuvYr",
                  spatial = best_sp,
                  spatiotemporal = best_st,
                  anisotropy = TRUE,
                  extra_time = unique(test$JuvYr[test$JuvYr>max(train$JuvYr)]), #Why is this necessary if the years are the same?
                  silent=TRUE)
    pred = predict(fit, test, re_form_iid = NA)$est
  }
  return(assign(paste0(mymod,n_years_ahead),
                pred))
}

proj_gam <- as.data.frame(do.call(cbind,lapply(FUN=myProj,n, mymod = "gam")))
proj_gam$mod <- "gam"
proj_gam <- cbind(df[df$JuvYr==max(df$JuvYr),],proj_gam[df$JuvYr==max(df$JuvYr),])

proj_rf <- as.data.frame(do.call(cbind,lapply(FUN=myProj,n, mymod = "rf")))
proj_rf$mod <- "rf"
proj_rf <- cbind(df[df$JuvYr==max(df$JuvYr),],proj_rf[df$JuvYr==max(df$JuvYr),])

proj_sdm <- exp(as.data.frame(do.call(cbind,lapply(FUN=myProj,n, mymod = "sdm"))))
proj_sdm$mod <- "sdm"
proj_sdm <- cbind(df[df$JuvYr==max(df$JuvYr),],proj_sdm[df$JuvYr==max(df$JuvYr),])

proj <- rbind(proj_gam,
              proj_rf,
              proj_sdm)
proj$diff_V1 <- proj$V1-proj$Juv.km

proj <- proj %>%
  gather(est, abs_error, c("diff_V1"))


library(viridisLite)
g <- ggplot(proj, aes(x=UTM_E/1000,y=UTM_N/1000,color=abs_error)) +
  geom_point(size=3) +
  scale_colour_viridis_c(alpha=0.7) +
  facet_grid(~mod) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
# g <- ggplot(proj, aes(x=Juv.km,y=value,color=est)) +
#   geom_point() +
#   facet_grid(~mod)

print(g)

