library(randomForest)
library(dplyr)

d = read.csv("JuvData.csv")
d$STRM_ORDER = as.factor(d$STRM_ORDER)
d$CLASS_Rank = as.factor(d$CLASS_Rank)

train = dplyr::filter(d, JuvYr < max(d$JuvYr))
test = dplyr::filter(d, JuvYr == max(d$JuvYr))

# Note MWMT_Index dropped because of NAs

grid_search = expand.grid(mtry = seq(3,15,2), ntree = seq(200,1000,100))

# find model with lowest out of sample rmse
grid_search$rmse = 0
for(i in 1:nrow(grid_search)) {
  fit = randomForest(Juv.km ~ STRM_ORDER + StrmSlope + 
                       MaxGradD + 
                       WidthM +
                       OUT_DIST + 
                       CLASS_Rank + 
                       StrmPow + 
                       MAnnSed + 
                       Barriers + 
                       SolMean + 
                       W3Dppt + 
                       SprPpt + 
                       IP_COHO, 
                     mtry = grid_search$mtry[i],
                     ntree = grid_search$ntree[i],
                     data=train)
  pred = predict(fit, test)
  grid_search$rmse[i] = sqrt(mean((pred - test$Juv.km)^2))
  
}

indx = which.min(grid_search$rmse)
fit = randomForest(Juv.km ~ STRM_ORDER + StrmSlope + 
                     MaxGradD + 
                     WidthM +
                     OUT_DIST + 
                     CLASS_Rank + 
                     StrmPow + 
                     MAnnSed + 
                     Barriers + 
                     SolMean + 
                     W3Dppt + 
                     SprPpt + 
                     IP_COHO, 
                   mtry = grid_search$mtry[indx],
                   ntree = grid_search$ntree[indx],
                   data=train)
# re-fit best model
saveRDS(fit, "output/rf_output.rds")
varImpPlot(fit)

# Repeat the grid search -- but this time include spatial variables

# find model with lowest out of sample rmse
grid_search$rmse = 0
for(i in 1:nrow(grid_search)) {
  fit = randomForest(Juv.km ~ STRM_ORDER + StrmSlope + 
                       MaxGradD + 
                       WidthM +
                       OUT_DIST + 
                       CLASS_Rank + 
                       StrmPow + 
                       MAnnSed + 
                       Barriers + 
                       SolMean + 
                       W3Dppt + 
                       SprPpt + 
                       IP_COHO + 
                       UTM_E + 
                       UTM_N, 
                     mtry = grid_search$mtry[i],
                     ntree = grid_search$ntree[i],
                     data=train)
  pred = predict(fit, test)
  grid_search$rmse[i] = sqrt(mean((pred - test$Juv.km)^2))
  
}

indx = which.min(grid_search$rmse)
fit = randomForest(Juv.km ~ STRM_ORDER + StrmSlope + 
                     MaxGradD + 
                     WidthM +
                     OUT_DIST + 
                     CLASS_Rank + 
                     StrmPow + 
                     MAnnSed + 
                     Barriers + 
                     SolMean + 
                     W3Dppt + 
                     SprPpt + 
                     IP_COHO + 
                     UTM_E + 
                     UTM_N, 
                   mtry = grid_search$mtry[indx],
                   ntree = grid_search$ntree[indx],
                   data=train)
# re-fit best model
saveRDS(fit, "output/rf_spatial_output.rds")
varImpPlot(fit)
