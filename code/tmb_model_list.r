m1 <- formula(gsub("[\r\n\t]", "",'Juv.km ~ 1'))



#Simplest spatial model
m2 <- formula(gsub("[\r\n\t]", "",'Juv.km ~ 1 
                            + exp(pos + 0 | ESU)'))

#Simplest spatial, temporal model
m3 <- formula(gsub("[\r\n\t]", "","Juv.km ~ 1 
                            + exp(pos + 0 | ESU)
                            + (1|JuvYr)"))

#Simplest spatiotemporal model
m4 <- formula(gsub("[\r\n\t]", "","Juv.km ~ 1 
                            + exp(pos + 0 | JuvYr)"))

#add the covariates with spatiotemporal model
m5 <- formula(gsub("[\r\n\t]", "","Juv.km ~ 1 
                      + scale(StrmSlope)
                      + scale(MaxGradD)
                      + scale(WidthM)
                      + scale(OUT_DIST)
                      + scale(MWMT_Index)
                      + scale(W3Dppt)
                      + scale(SprPpt)
                      + scale(IP_COHO)
                      + as.factor(Stratum)
                      + as.factor(STRM_ORDER)
                      + as.factor(CLASS_Rank)
                      + exp(pos + 0 | JuvYr)"))

#add the covariates with spatiotemporal model
m6 <- formula(gsub("[\r\n\t]", "","Juv.km ~ 1 
                      + scale(StrmSlope)
                      + scale(MaxGradD)
                      + scale(WidthM)
                      + scale(OUT_DIST)
                      + scale(MWMT_Index)
                      + scale(W3Dppt)
                      + scale(SprPpt)
                      + scale(IP_COHO)
                      + as.factor(Stratum)
                      + as.factor(STRM_ORDER)
                      + as.factor(CLASS_Rank)
                      + (1|JuvYr)
                      + exp(pos + 0 | ESU)"))

m7 <- formula(gsub("[\r\n\t]", "","Juv.km~ 1 
                      + scale(StrmSlope)
                      + scale(MaxGradD)
                      + scale(WidthM)
                      + scale(OUT_DIST)
                      + scale(MWMT_Index)
                      + scale(W3Dppt)
                      + scale(SprPpt)
                      + scale(IP_COHO)
                      + as.factor(Stratum)
                      + as.factor(STRM_ORDER)
                      + as.factor(CLASS_Rank)"))

m8 <- formula(gsub("[\r\n\t]", "","Juv.km ~ 1 
                      + scale(UTM_N)
                      + scale(UTM_E)
                      + scale(StrmSlope)
                      + scale(MaxGradD)
                      + scale(WidthM)
                      + scale(OUT_DIST)
                      + scale(MWMT_Index)
                      + scale(W3Dppt)
                      + scale(SprPpt)
                      + scale(IP_COHO)
                      + as.factor(Stratum)
                      + as.factor(STRM_ORDER)
                      + as.factor(CLASS_Rank)"))
