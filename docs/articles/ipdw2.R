## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.pos = 'center')

## ----create_polygons, message = FALSE------------------------------------
library("ipdw")
library("geoR")
data(kattegat)
katproj <- c("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

pols1 <- Polygons(list(Polygon(kattegat$dk[1])), "pol1")
pols2 <- Polygons(list(Polygon(kattegat$dk[2])), "pol2")
pols3 <- Polygons(list(Polygon(kattegat$dk[3])), "pol3")
pols4 <- Polygons(list(Polygon(kattegat$dk[4])), "pol4")
pols5 <- Polygons(list(Polygon(kattegat$dk[5])), "pol5")
pols6 <- Polygons(list(Polygon(kattegat$dk[6])), "pol6")
pols7 <- Polygons(list(Polygon(kattegat$dk[7])), "pol7")
pols8 <- Polygons(list(Polygon(kattegat$dk[8])), "pol8")
pols9 <- Polygons(list(Polygon(kattegat$dk[9])), "pol9")
pols10 <- Polygons(list(Polygon(kattegat$dk[10])), "pol10")
pols11 <- Polygons(list(Polygon(kattegat$dk[11])), "pol11")
pols12 <- Polygons(list(Polygon(kattegat$dk[12])), "pol12")
pols <- SpatialPolygons(list(pols1, pols2, pols3, pols4, pols5, pols6, 
                           pols7, pols8, pols9, pols10, pols11, pols12), 1:12)

## ----create_costraster---------------------------------------------------
projection(pols) <- katproj
costras <- costrasterGen(kattegat$coords, pols, extent = "pnts", katproj)
#insert contiguous barrier
costras[160:170,1:80] <- 10000

## ----avg_nearest_neigh, message = FALSE----------------------------------
#find average nearest neighbor
library(spatstat)
W <- owin(range(kattegat$coords[,1]), range(kattegat$coords[,2]))
kat.pp <- ppp(kattegat$coords[,1], kattegat$coords[,2], window = W)
mean.neighdist <- mean(nndist(kat.pp))

#grid building
gridsize <- mean.neighdist*2
grainscale.fac <- gridsize/res(costras)[1]
gridras <- aggregate(costras, fact = grainscale.fac)
gridpol <- rasterToPolygons(gridras)
gridpol$value <- row.names(gridpol)

#spatial join
kat.df <- data.frame(kattegat)
coordinates(kat.df) <- ~x.utm + y.utm
projection(kat.df) <- katproj
fulldataset.over <- over(kat.df, gridpol)
fulldataset.over <- cbind(data.frame(fulldataset.over), data.frame(kat.df))

#grid selection
set.seed(2)
gridlev <- unique(fulldataset.over$value)
for(i in seq_along(gridlev)){
  activesub <- subset(fulldataset.over, fulldataset.over$value == gridlev[i])
  selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
  if(i == 1){
    training <- activesub[selectnum,]
  }
  else{
    training <- rbind(training, activesub[selectnum,])
  }
}

## ----split_training_validation-------------------------------------------
validate <- fulldataset.over[!(row.names(fulldataset.over) %in% 
															 	row.names(training)),]
xy <- cbind(training$x.utm, training$y.utm)
training <- SpatialPointsDataFrame(xy, training)
xy <- cbind(validate$x.utm, validate$y.utm)
validate <- SpatialPointsDataFrame(xy, validate)
projection(training) <- katproj
projection(validate) <- katproj

## ----plot_cost_raster, fig.cap = "<strong>Figure 1: Cost raster representing the high cost of travel through land areas. Training and validation points are shown in black and red respectively.</strong>", fig.height = 6, fig.width = 5----

plot(costras)
points(training)
points(validate, col = "red")

## ----interpolate, cache = FALSE, message = FALSE, results = 'hide'-------
paramlist <- c("data")
final.ipdw <- ipdw(training, costras, range = mean.neighdist * 10, paramlist,
									 overlapped = TRUE)

## ----plot_interpolation, fig.cap = "<strong>Figure 2: Interpolated salinity surface by IPDW.</strong>", fig.height = 6, fig.width = 5----
plot(final.ipdw, main = "Kattegat salinity (ppt)")

## ----create_idw----------------------------------------------------------
idw.grid <- rasterToPoints(costras, fun = function(x){x<10000}, spatial = TRUE)
gridded(idw.grid) <- TRUE
kat.idw <- gstat::idw(data~1, training, idw.grid, maxdist = mean.neighdist*10,
											debug.level = 0)
final.idw <- raster(kat.idw)

## ----plot_ipdw_vs_idw, fig.cap = "<strong>Figure 3: Comparison between IPDW and IDW outputs. Note the overestimation of salinity on the upstream (south) side of the contiguous barrier.</strong>", fig.width = 6, fig.height = 4----
par(mfrow = c(1, 3), mar = c(5.1, 4.1, 4.1, 5.1))
plot(final.ipdw, main = "IPDW")
plot(final.idw, main = "IDW")
plot(final.idw-final.ipdw,  main = "IDW versus IPDW")

## ----generate_validation-------------------------------------------------
measured.spdf <- data.frame(validate$data)
coordinates(measured.spdf) <- coordinates(validate)

valid.ipdw <- errorGen(final.ipdw,measured.spdf, measured.spdf@data)
valid.idw <- errorGen(final.idw, measured.spdf, measured.spdf@data)

## ----plot_validation, fig.cap = "<strong>Figure 4: Comparison between IPDW and IDW interpolation error.  A one-to-one line and best-fit line are shown in black and red respectively.</strong>", fig.width = 8, fig.height = 5----
par(mfrow = c(1, 2))
valid.ipdw <- errorGen(final.ipdw, measured.spdf, measured.spdf@data, 
											 plot = TRUE, title = "IPDW")
valid.idw <- errorGen(final.idw, measured.spdf, measured.spdf@data, 
											plot = TRUE, title = "IDW")

