load("C:/noaa/projects/ODFW-Coho/output/output.rData")


marVars <- c('WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO', 
            'SolMean','StrmSlope',
            'OUT_DIST')

df_raw <- read.csv("juvData.csv")

par(mfrow=c(2,2))

for(i in marVars[1:4]){
  df_tmp <- df
  testYr <- 2019
  bool <- df_tmp$JuvYr==testYr
  df_tmp$UTM_E_km[bool] <- mean(df$UTM_E_km[bool])
  df_tmp$UTM_N_km[bool] <- mean(df$UTM_N_km[bool])
  df_tmp$STRM_ORDER[bool] <- 1
  df_tmp$fSTRM_ORDER[bool] <- as.factor(df_tmp$STRM_ORDER[bool])
  df_tmp[bool,marVars] <- 0    
  df_tmp[bool,i] <- seq(min(df[,i]),max(df[,i]),length.out=sum(bool))    
  p <- list(gam=exp(predict(output$exploratory$gam$best_fit,df_tmp[bool,])),
             # rf = partial(output$exploratory$rf$best_fit,pred.var=as.character(i),ice=TRUE),
             sdm = exp(predict(output$exploratory$sdm$best_fit,df_tmp)$est[bool]))
  
  xx <- df_tmp[bool,i]*sd(na.omit(df_raw[bool,i]))+mean(na.omit(df_raw[bool,i]))
  
  # autoplot(p$rf, alpha=0.1)
  
  q <- quantile(df[,i]*sd(na.omit(df_raw[bool,i]))+mean(na.omit(df_raw[bool,i])), probs=c(0.1,0.9))
  matplot(df[,i]*sd(na.omit(df_raw[bool,i]))+mean(na.omit(df_raw[bool,i])),
            log(df$Juv.km),
            pch=16,
            xlim = q,
            col=alpha("lightgrey",0.2))
  matlines(xx,
       log(cbind(p$gam,p$sdm)),
       type="l",
       lty=1,
       col=1:2,
       lwd=3,
       xlab=i,
       ylab="Juv/km")
  #Just pick the 90% quartiles for the data
  #Plot the s.e for the lines.
  
}


