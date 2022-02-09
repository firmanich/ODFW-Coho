library(glmmTMB)
library(DHARMa)

#Starting with the same data as Eric
d = read.csv("JuvData.csv")
d = dplyr::filter(d, !is.na(MWMT_Index))
d$STRM_ORDER = as.factor(d$STRM_ORDER)
d$CLASS_Rank = as.factor(d$CLASS_Rank)
d$ESU <- as.factor(d$ESU)
d$SiteID <- as.factor(d$SiteID)

#Position data for spatial analysis
#Convert position data in coordinate, but rescale and try binning
#to reduce the number of 'nodes'
d$pos <- numFactor(as.vector(round(d$UTM_E/100000,1)), 
                          as.vector(round(d$UTM_N/1000000,2)))


#Training data
d_j <- d[d$JuvYr<max(d$JuvYr),]
#Necessary for TMB ar and exp processes
d_j$JuvYr <- as.factor(d_j$JuvYr)

#Predictive data
d_p <- d[d$JuvYr==max(d$JuvYr),]


#Store output
f <- list()

#Save space and make it cleaner - put models in tmb_model_list.r
source(paste0(getwd(),'/code/tmb_model_list.r'))

# #Loop over a few models
# icnt <- 0
# for(m in 1:2){
#   if(m == 2){
#     for(i in 1:8){
#       icnt <- icnt + 1
#       f[[icnt]] <- glmmTMB(get(paste0('m',i)),
#                           data=d_j[,],
#                           verbose=TRUE,
#                           family=tweedie)
#     }
#   }
#   if(m == 1){
#     for(i in 1:8){
#       icnt <- icnt + 1
#       f[[icnt]] <- glmmTMB(get(paste0('m',i)),
#                           data=d_j[,],
#                           ziformula = ~1,
#                           verbose=TRUE,
#                           family=ziGamma(link="log"),
#                           start = log(100))
#     }
#   }
# }
# 
# myAIC <- sapply(f,AIC)
# 
# mods <- list()
# for(i in 1:7)
#   mods[[i]] <- get(paste0("m",i))
# 
# mod_names <- c('mean',
#                'pos',
#                'pos + time',
#                'pos | time',
#                'ENV + pos | time',
#                'ENV + pos + time',
#                'ENV',
#                'ENV + fixed.pos')
# 
# AICtab <- data.frame(mods = mod_names, #sapply(mods,function(x){return(Reduce(paste,deparse(x)))}), 
#                      ziGamma = round(myAIC[1:8],0)-min(round(myAIC[1:8],0)),
#                      Tweedie = round(myAIC[9:16],0)- min(round(myAIC[9:16],0)))
# save(AICtab, file="AICtab.rData")
# 
# #Get the residual information
# res <- list()
# for(i in 1:16) {res[[i]] <- simulateResiduals(f[[i]], plot=F)}
# save(res, file="res.rData")
# 
#Simplest spatial, temporal model
tmp.m <- formula(gsub("[\r\n\t]", "","Juv.km ~ 1 
                            + (1|JuvYr)
                            # + exp(pos + 0 | ESU)
                      "))

m.out <- glmmTMB(tmp.m,
                     data=d_j[,],
                     # ziformula = ~(1|JuvYr),
                     verbose=TRUE,
                     family=ziGamma,
                 # ,start = log(100)
                 )

p <- ggplot(d[d$Juv.km>0,], aes(x=log(Juv.km))) + 
  geom_histogram(binwidth=1) +
  facet_wrap(~as.factor(Stratum))

print(p)

simulateResiduals(m.out, plot=T, n = 1000)
