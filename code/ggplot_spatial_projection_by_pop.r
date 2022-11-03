library(sp)
library(glmmTMB)
library(raster)
library(ggplot2)
library(viridis)
library(viridisLite)
library(cowplot)
library(RANN)

#2e) Now you need the covariates for your model
stage <- "rear" #rear or Spwn

png(paste0('output/ggplot_spatial_projection_by_pop_',stage,'.png'),
    height = 400, width = 800, pointsize = 14)

#Grab the data for stage
df <- function_wrangle_data(stage=stage,
                            dir = root)
load(paste0("output/output_",stage,".rData"))

odfw_est <- read.csv("./data/ESU_Estimates.csv")

#Subset by unique locations, non-duplicated df
nd.df <- df[!duplicated(df[,c('UTM_E_km','UTM_N_km','PopGrp')]),c('UTM_E_km','UTM_N_km','PopGrp')]
nd <- nrow(nd.df)
nd.df$UTM_E <- nd.df$UTM_E_km*1000
nd.df$UTM_N <- nd.df$UTM_N_km*1000
nd.df[,names(df)[!names(df)%in%names(nd.df)]] <- 0 #add all of the column heading
nd.df <- do.call("rbind", replicate(length(unique(df$yr)), nd.df, simplify = FALSE))
nd.df$STRM_ORDER <- 1
nd.df$fSTRM_ORDER <- as.factor(1)
nd.df$yr <- rep(unique(df$yr),each=nd)
nd.df$fYr <- as.factor(nd.df$yr)

#Use kmeans to map the stream in the original data frame to 
#the prediction dataframe
# kmean <- nn2(df[,c('UTM_E','UTM_N')],nd.df[,c('UTM_E','UTM_N')], k=1)

coVars <- c('WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO',
            'SolMean','StrmSlope',
            'OUT_DIST','STRM_ORDER' 
)


for(iii in coVars){
  for(y in unique(nd.df$yr)){
    kmean <- nn2(df[df$yr==y,c('UTM_E','UTM_N')],nd.df[nd.df$yr==y,c('UTM_E','UTM_N')], k=1)
    nd.df[nd.df$yr==y,iii] <- df[kmean$nn.idx,iii]
  }
}


fit <- output$exploratory$gam$best_fit
# frm <- mod_search$form$gam$m1
# fit <-  gam(dens ~ fSTRM_ORDER + s(StrmSlope, k = 4) + s(WidthM, k = 4) +
#               s(OUT_DIST, k = 4) + s(SolMean, k = 4) + s(MWMT_Index, k = 4) +
#               s(W3Dppt, k = 4) + s(StrmPow, k = 4) + s(SprPpt, k = 4) +
#               s(IP_COHO, k = 4), data=df, family = "tw")
#Predict the year in questions
# p <-  predict(fit,nd.df, family = "tw")
# pred <- exp(predict(fit, df))#
pred1 <- exp(predict(fit, nd.df))
p <- cbind(nd.df,pred1)
p$mod <- "GAM \n (mgcv)"

ff <- output$exploratory$rf$best_mod
mtry <- output$exploratory$rf$best_mtry
ntree <- output$exploratory$rf$best_ntree
fit <- randomForest(ff,
                    mtry = mtry,
                    ntree = ntree,
                    data = df)
pred1 <- predict(fit, nd.df)
tmp <- cbind(nd.df,pred1)
tmp$mod <- "Random forest \n (randomForest)"
p <- rbind(p,tmp)
# 

fit <- output$exploratory$sdm$best_fit
pred1 <-exp(predict(fit, nd.df)$est)
# tmp <- cbind(nd.df,pred$est)
tmp <- cbind(nd.df,pred1)
names(tmp)[ncol(tmp)] <- "pred1"
tmp$mod <- "GLMM \n (sdmTMB)"
p <- rbind(p,tmp)

ag <- aggregate(list(est = p$pred1), by=list(yr = p$yr, mod = p$mod, pop = p$PopGrp), sum)
if(stage=="Spwn"){
  odfw <- data.frame(yr=odfw_est$Brood.Year,
                     mod = "Agg. odfw",
                     pop = "Agg. odfw",
                     'Estimate' = odfw_est$Spawners)
}
if(stage=="rear"){
  odfw <- data.frame(yr = odfw_est$Parr.Year,
                     mod = "Agg. odfw",
                     pop = "Agg. odfw",
                     'Estimate' = odfw_est$TotalParr)
}

names(ag)[ncol(ag)] <- "Estimate"
ag <- rbind(ag,
            odfw[odfw$yr%in%ag$yr,])
ag$stage <- stage



g <- ggplot(ag[,], aes(x = yr,
                     y = Estimate,
                     colour = mod)) +
  geom_line() +
  facet_wrap(~pop, scales = "free_y")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))+
  labs(color = "Model estimate")+
  ylab("Estimate") +
  xlab("Year")
# 
print(g)

dev.off()
