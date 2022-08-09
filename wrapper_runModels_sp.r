library(dplyr)
library(sdmTMB)

root <- getwd()

# The dropped variables
#Barriers, mean annual sediment, max gad D, class rank, 


myVars <- c('AUC_Mi','UTM_E','UTM_N',
            'WidthM','W3Dppt',
            'MWMT_Index','StrmPow',
            'SprPpt','IP_COHO', 
            'SolMean','StrmSlope',
            'OUT_DIST','JuvYr',
            'STRM_ORDER')

df <- read.csv("SpawnData.csv")

#Wrangling
#All models use the same data.
scale_this <- function(x) as.vector(scale(x))
df <- df %>%
  dplyr::select(all_of(myVars)) %>% #grab myVars from above
  mutate(MWMT_Index= ifelse(is.na(MWMT_Index), mean(MWMT_Index, na.rm=TRUE), MWMT_Index)) %>% #get rid of NAs
  mutate(UTM_E_km = UTM_E/1000, #Rescale
         UTM_N_km = UTM_N/1000, #Rescale
         fJuvYr = as.factor(JuvYr),
         fSTRM_ORDER = as.factor(STRM_ORDER),
         StrmPow = scale_this(StrmPow),
         WidthM = scale_this(WidthM),
         W3Dppt = scale_this(W3Dppt),
         MWMT_Index = scale_this(MWMT_Index),
         SprPpt = scale_this(SprPpt),
         IP_COHO = scale_this(IP_COHO), 
         SolMean = scale_this(SolMean),
         StrmSlope = scale_this(StrmSlope),
         OUT_DIST = scale_this(OUT_DIST)
  )



#run through the models 
for(project in c(FALSE)){ #Projections vs. model exploration
  # project <- proj
  if(project){
    #Number of years to project ahead
    n_years_ahead = c(0,1,2)# can be 0, 1, 2
    # use avg predictions for last 5 years to train model
    n_test = 5
  }else{
    # use last year for test model
    n_years_ahead = 0# Use all of the data
    # only look at the last year of prediction in exploration model
    n_test = 1
  }
  test_years = seq(max(df$JuvYr)-n_test+1, max(df$JuvYr))
  

  #Run through models
  for(mod in c("rf")){
    #Build all of the different models
    #get the old output
    source("wrapper_mod_searches.r")
    source(paste0("C:/noaa/projects/ODFW-coho/code/exploratory_",mod,"_chasco.r"))
  }
}

