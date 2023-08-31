Life_stage <- "Spwn"


load(paste0("C:/noaa/projects/ODFW-Coho/output/temporal_output_",Life_stage,"_2021.rData"))



marVars <- c('StrmSlope','WidthM','SolMean','IP_COHO','OUT_DIST','StrmPow','MWMT_Index','W3Dppt','SprPpt')

# df_raw <- read.csv("juvData.csv")

par(mfrow=c(3,3))
g <- list()
icnt<- 1
testYr <- 2021
x <- list()
 
for(i in marVars){
  # i <- marVars[1]
  df_tmp <-   df <- function_wrangle_data(stage=Life_stage, 
                                          dir = getwd(),
                                          maxYr = 2021)

  nyears <- unique(df$yr[df$yr!=testYr])
  bool <- df_tmp$yr==testYr
  df_tmp$UTM_E_km[bool] <- mean(df$UTM_E_km[bool])
  df_tmp$UTM_N_km[bool] <- mean(df$UTM_N_km[bool])
  df_tmp[bool,marVars] <- 0    
  df_tmp$STRM_ORDER[bool] <- 3
  df_tmp$fSTRM_ORDER[bool] <- as.factor(df_tmp$STRM_ORDER[bool])
  df_tmp[bool,i] <- seq(min(df[,i]),max(df[,i]),length.out=sum(bool))    
  
  df_nbool <- df_tmp[!bool,]  
  df_tmp <- df_tmp[bool,]  
  
  df_tmp2 <- df_tmp[1:length(nyears),]
  df_tmp2$yr <- nyears
  df_tmp2$fYr <- as.factor(df_tmp2$yr)
  df_tmp <- rbind(df_tmp2, df_tmp)
  
  x[[icnt]] <- predict(output$exploratory$sdm$best_fit, newdata =  df_tmp, se_fit = TRUE)
  x[[icnt]]$x <- df_tmp[,i]
  
  x[[icnt]] <- x[[icnt]][x[[icnt]][,i]>(-2) & x[[icnt]][,i]<(5), ]
  
  print(paste(i, dim(x)))
  q <- quantile(x[[icnt]]$est, probs=c(0.05,0.95))
  library(ggplot2)
  g[[icnt]] <- ggplot2::ggplot(x[[icnt]][x[[icnt]]$yr==2021,],aes(x = x, y = exp(est - 0.5*est_se^2))) +
    geom_line() +
    geom_ribbon(aes(ymin = exp(est - 0.5*est_se^2) - 1.64*exp(est)*est_se, 
                    ymax = exp(est - 0.5*est_se^2) + 1.64*exp(est)* est_se),
                alpha = 0.2) +
    theme_bw() +
    xlab(paste(i))
  
  # hist()
  
  if(Life_stage=="Spwn"){
    g[[icnt]] <- g[[icnt]] +
      ylim(0,100) +
      ylab("Spawner density (#/mi)")
  }else{
    g[[icnt]] <- g[[icnt]] +
      ylim(0,1250) +
      ylab("Juvenile density (#/km)")
  }

  icnt <- icnt + 1
}

gg <- ggpubr::ggarrange(plotlist = g)

ggsave(file = paste0("./output/ggplot_marginal_plot_",Life_stage,".png"), 
       gg, 
       device = "png", dpi = 500, height = 6, width = 6, units="in")


print(gg)

