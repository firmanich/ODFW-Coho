function_wrangle_data <- function(stage=NA,
                                  dir = root){
  

  library(dplyr)
  library(tidyr)
  #Read in the juvenile data, change AUC.Mi to dens
  juv <- read.csv(paste0('C:/noaa/LARGE_Data/JuvData.csv'), 
                  header=TRUE,
                  dec=".",
                  stringsAsFactors = FALSE) %>% 
    mutate('dens' = Juv.km) %>% 
    mutate('yr' = as.integer(JuvYr)) %>% 
    mutate(UTM_E = as.numeric(UTM_E)) %>%
    mutate(UTM_N = as.numeric(UTM_N)) %>%
    filter_at(vars(dens,UTM_E,UTM_N), all_vars(!is.na(.)))#get rid of anything without a density
    # filter_at(vars(UTM_E, UTM_N), all_vars(!is.na(.)))
  
  #Read in the spawner data, change AUC.Mi to dens
  sp <- read.csv(paste0('C:/noaa/LARGE_Data/SpawnData.csv'),
                 header=TRUE,
                 dec=".",
                 stringsAsFactors = FALSE) %>%
    mutate('dens' = AUC.Mi) %>% 
    mutate('yr' = SpwnYr) %>% 
    mutate(UTM_E = as.numeric(as.character(UTM_E))) %>%
    mutate(UTM_N = as.numeric(UTM_N)) %>%
    filter_at(vars(dens), all_vars(!is.na(.))) #get rid of anything without a density
    # filter_at(vars(UTM_E, UTM_N), all_vars(!is.na(.)))
  
  #row bind the data based on common column headings
  depVars <- c('STRM_ORDER','LifeStage','dens','yr','PopGrp','ID_Num')
  coVars <- c('UTM_E','UTM_N'
              ,'WidthM','W3Dppt',
              'MWMT_Index','StrmPow',
              'SprPpt','IP_COHO', 
              'SolMean','StrmSlope',
              'OUT_DIST'
  )
  
  #Cheap function
  scale_this <- function(x) as.vector(scale(x))
  my_factor <- function(x) as.factor(x)
  #combine, rescale, and fill in some missing vals
  df <- bind_rows(juv,sp) %>% #Not sure why I decided to combine these and then subset
    dplyr::select(all_of(c(depVars,coVars)))%>% #grab myVars from above
    filter_at(vars(dens,UTM_E,UTM_N), all_vars(!is.na(.))) %>% #get rid of anything without a density or UTM
    filter(LifeStage==!!stage) %>% #grab a particular life stage
    mutate_at(all_of(coVars), ~replace_na(.,mean(., na.rm = TRUE))) %>% #get rid of NAs, a little TOO CLUTCHY
    mutate(UTM_E_km = UTM_E/1000, #Rescale
           UTM_N_km = UTM_N/1000, #Rescale
           # ID_Num = ID_Num,
           fYr = as.factor(yr),
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
  
  # df <- df[df$UTM_E_km!=0,]
  df <- df[abs(df$W3Dppt)<=4,]
  df <- na.omit(df) #Necessary to get Spwn data to work.
  
  return(df)
  
}
