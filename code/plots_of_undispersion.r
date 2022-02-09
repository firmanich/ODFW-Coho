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


p <- ggplot(d[d$Juv.km>0,], aes(x=log(Juv.km))) + 
  geom_histogram(binwidth=1) +
  facet_wrap(~as.factor(Stratum))

pdf(paste0(getwd(),"/output/plots_of_underdispersion_stratum.pdf"), height=6,width=6)
  print(p)
dev.off()

p <- ggplot(d[d$Juv.km>0,], aes(x=log(Juv.km))) + 
  geom_histogram(binwidth=1) +
  facet_wrap(~as.factor(JuvYr))

pdf(paste0(getwd(),"/output/plots_of_underdispersion_JuvYr.pdf"), height=6,width=6)
  print(p)
dev.off()

p <- ggplot(d[d$Juv.km>0,], aes(x=log(Juv.km))) + 
  geom_histogram(binwidth=1) +
  facet_wrap(~as.factor(PopGrp))

pdf(paste0(getwd(),"/output/plots_of_underdispersion_PopGrp.pdf"), height=6,width=6)
print(p)
dev.off()
