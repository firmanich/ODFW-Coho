library(ggplot2)

pred_spatial <- function(x, years) {
  #Create the UTM grid after rescaling the axes
  myGrid <- expand.grid(x=unique(round(d_j$UTM_E/100000,1)),
                        y=unique(round(d_j$UTM_N/1000000,2)))
  #Create the position
  p_pos <- numFactor(as.vector(myGrid[,1]),as.vector(myGrid[,2]))
  
  #Create the prediction data
  p_data <- data.frame(pos=p_pos,
                       StrmSlope = rep(0,length(p_pos)*length(years)),
                       MaxGradD = rep(0,length(p_pos)*length(years)),
                       WidthM = rep(0,length(p_pos)*length(years)),
                       OUT_DIST = rep(0,length(p_pos)*length(years)),
                       MWMT_Index = rep(0,length(p_pos)*length(years)),
                       W3Dppt = rep(0,length(p_pos)*length(years)),
                       SprPpt = rep(0,length(p_pos)*length(years)),
                       IP_COHO = rep(0,length(p_pos)*length(years)),
                       Stratum = as.factor(rep("Mid-Coast",length(p_pos)*length(years))),
                       STRM_ORDER = as.factor(rep("4",length(p_pos)*length(years))),
                       CLASS_Rank = as.factor(rep("1",length(p_pos)*length(years))),
                       UTM_E = rep(myGrid[,1], length(years)),
                       UTM_N = rep(myGrid[,2], length(years)),
                       JuvYr = rep(years,each=length(p_pos)),
                       ESU = as.factor(rep("OC",length(p_pos))))
  
  print(length(years))
  print(years)
  
  #Make the prediction
  pred <- predict(f[[x]],newdata=p_data, type="response", allow.new.levels=TRUE)          
  
  #Add the predictions to the data.frame
  p_data$p <- pred
  
  return(p_data)
}
