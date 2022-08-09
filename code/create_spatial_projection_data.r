library(sp)
library(glmmTMB)
library(raster)
library(ggplot2)
#Predict the spatiotemporal effects for 2019


#Before we can plot the predictions, we have to do a couple of this
#1) Create the grid,
#1a) Create the grid
x_seq <- seq(min(df$UTM_E_km),max(df$UTM_E_km),by = 2) #UTM N
y_seq <- seq(min(df$UTM_N_km),max(df$UTM_N_km),by = 5) #UTM N
vizloc_xy = expand.grid( x=x_seq, y=y_seq) #Spatial field
vizloc_xy$p <- 1
#1b) transform gridded data into coordinates and raster
coordinates(vizloc_xy) <- ~x+y
# coerce to SpatialPixelsDataFrame
gridded(vizloc_xy) <- TRUE
# coerce to raster
rasterDF <- raster(vizloc_xy)

#2) Remove all grid cells from rasterDF that are outside of OC ESU
#2a) Get Oregon spatial ESU
dir <- "C:/NOAA/PROJECTS/ODFW-Coho/"
setwd(paste0(dir,"shp/"))
shape = rgdal::readOGR(".","OregonCoast_coho_ESU-utm83") #will load the shapefile to your dataset.
OR_ESU <- as.data.frame(shape@polygons[[1]]@Polygons[[1]]@coords)
names(OR_ESU) <- c("X","Y")
setwd(dir) #restore root directory
coastXY <- data.frame(X=OR_ESU$X,Y=OR_ESU$Y)/1000 #coastline polygon
Sr1 = Polygon(cbind(OR_ESU$X/1000,OR_ESU$Y/1000))
SpP = SpatialPolygons(list(Polygons(list(Sr1), "s1")), 1:1) #1:1 is the trick, can't say 1


#2b)#Get Oregon boundary
LatLong2UTM <- function(x,y,ID,zone){ #I'm sure this function is redudant with sdmTMB
  xy <- data.frame(ID=ID, X = x, Y=y)
  coordinates(xy) <- c("X", "Y")
  proj4string(xy) <- CRS("+proj=longlat +datum=WGS84")  ## for example
  res <- spTransform(xy, CRS(paste("+proj=utm +zone=", zone, "ellps=WG84", sep='')))
  return(as.data.frame(res))
}

or <- map_data("state","oregon")
or_utm <- add_utm_columns(data.frame("longitude"=or$long,"latitude"=or$lat))
# or_xy <- LatLong2UTM(or$long,or$lat,or$order,10)
# Sr1 = Polygon(cbind(xy$X,xy$Y))


#2c) Create the OC ESU mask
myMask <- mask(rasterDF,SpP)
tmp <- as.data.frame(cbind(coordinates(myMask),p = myMask@data@values))
predmask <- na.omit(tmp)
plot(predmask$x,predmask$y)

pop_poly <- data.frame(UTM_E_km = NA, 
                       UTM_N_km = NA)

#2e) Now you need the covariates for your model
p.df <- data.frame("UTM_E_km" = predmask$x, "UTM_N_km" = predmask$y) #start with population groups
p.df$UTM_E <- p.df$UTM_E_km*1000
p.df$UTM_N <- p.df$UTM_N_km*1000
p.df[,names(df)[!names(df)%in%names(p.df)]] <- 0 #add all of the column heading
p.df <- do.call("rbind", replicate(length(unique(df$JuvYr)), p.df, simplify = FALSE))
p.df$STRM_ORDER <- as.factor(1)
p.df$fSTRM_ORDER <- as.factor(1)
p.df$JuvYr <- rep(unique(df$JuvYr),each=nrow(predmask))
p.df$fJuvYr <- as.factor(p.df$JuvYr)
load("output/output.rData")


fit <- output$exploratory$gam$best_fit
pred <- predict(fit, newdata=p.df)#[p.df$JuvYr%in%unique(df$JuvYr),])
p <- cbind(p.df,pred) 
p$mod <- "gam"

fit <- output$exploratory$rf$best_fit
pred <- log(predict(fit, newdata=p.df))
tmp <- cbind(p.df,pred)
tmp$mod <- "rf"
p <- rbind(p,tmp)

fit <- output$exploratory$sdm$best_fit
pred<-predict(fit, newdata=p.df)
tmp <- cbind(p.df,pred$est)
names(tmp)[ncol(tmp)] <- "pred"
tmp$mod <- "sdm"
p <- rbind(p,tmp)

minY <- 2016
g <- ggplot(p[p$JuvYr>=minY,], aes(x=UTM_E_km,y=UTM_N_km, fill=(pred)), alpha=0.2) + 
  facet_grid(JuvYr~mod) + 
  geom_raster() +
  scale_fill_viridis_c() +
  ylab("Northing (km)") +
  xlab("Easting (km)") +
  geom_point(data = df[df$JuvYr>=minY,], aes(x=UTM_E_km,y=UTM_N_km, color = log(Juv.km)), inherit.aes = FALSE, shape = 1) +
  scale_colour_viridis_c()

png("output/ggplot_model_pred_comparison.png", height = 800, width = 600)

