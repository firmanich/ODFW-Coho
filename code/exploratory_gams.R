library(mgcv)
library(dplyr)

d = read.csv("JuvData.csv")
train = dplyr::filter(d, JuvYr < max(d$JuvYr))
test = dplyr::filter(d, JuvYr == max(d$JuvYr))

# create dataframe for predictions
pred = data.frame("gam_spatial" = rep(0,nrow(test)))

# First fit a naive model with only spatial effects
fit = list()
fit[[1]] = gam(Juv.km ~ s(UTM_E,UTM_N), data=train, family = "nb")

# Second second naive model with spatiotemporal effects
fit[[2]] = gam(Juv.km ~ s(UTM_E,UTM_N, JuvYr), data=train, family = "nb")

# Third, fit some models with covariates -- but without the spatial effects
fit[[3]] = gam(Juv.km ~ as.factor(STRM_ORDER) + s(StrmSlope,k=4) + 
            s(MaxGradD,k=4) + 
            s(WidthM,k=4) +
            s(OUT_DIST,k=4) + 
            as.factor(CLASS_Rank) + 
            s(StrmPow,k=4) + 
            s(MAnnSed,k=4) + 
            s(Barriers,k=4) + 
            s(SolMean,k=4) + 
            s(MWMT_Index,k=4) + 
            s(W3Dppt,k=4) + 
            s(SprPpt,k=4) + 
            s(IP_COHO,k=4), 
          data=train, family = "nb")

fit[[4]] = gam(Juv.km ~ as.factor(STRM_ORDER) + s(StrmSlope,k=4) + 
            s(MaxGradD,k=4) + 
            s(WidthM,k=4) +
            s(OUT_DIST,k=4) + 
            as.factor(CLASS_Rank) + 
            s(MWMT_Index,k=4) + 
            s(W3Dppt,k=4) + 
            s(IP_COHO,k=4), 
          data=train, family = "nb")

saveRDS(fit,"output/gam_output.rds")

# make predictions for held out data frame
pred$gam_spatial = predict(fit[[1]], newdata = test, type="response")
pred$gam_spatiotemp = predict(fit[[2]], newdata = test, type="response")
pred$gam_full = predict(fit[[3]], newdata = test, type="response")
pred$gam_reduced = predict(fit[[4]], newdata = test, type="response")

indx = which(complete.cases(pred)==TRUE)
for(i in 1:length(fit)) print(sqrt(mean((test$Juv.km[indx]-pred[indx,i])^2)))

