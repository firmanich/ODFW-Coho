#Store model predictions
mod_pred <- list()

icnt <- 0

#Years of interest
years <- seq(1998,2016,6)

mod_names <- c('pos',
               'pos + time',
               'pos | time',
               'ENV + pos | time',
               'ENV + pos + time')
  
for(i in 10:14){ #Loop over models #ziGamma 1:8, #Tweedie 9:16
  icnt <- icnt + 1
  x <- pred_spatial(i, years) #See predict_spatial.R
  x$mod <- mod_names[icnt]
  mod_pred[[icnt]] <- x
}

#Convert list to data.frame
df <- as.data.frame(do.call(rbind, lapply(mod_pred, as.data.frame)))


#Use ggplot
p <- ggplot() +
  geom_tile(data = df[,], 
            aes(x = UTM_E*10, 
                y = UTM_N*1000, 
                fill = (p),
            )) + 
  facet_wrap(mod~JuvYr, ncol = length(years)) +
  scale_fill_gradient("Juv/km",
                      low = 'grey', high = 'blue',
                      na.value = NA) + 
  ylab('Northing (km)') + 
  xlab('Easting (km)') 


# pdf("tmbGlmm_spatial_plot_ziGamma.pdf", height  =8, width = 6)
#   print(p)
# dev.off()
# 
pdf("tmbGlmm_spatial_plot_Tweedie.pdf", height  =8, width = 6)
  print(p)
dev.off()

