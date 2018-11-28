library(geoR)
library(sp)
library(rgdal)
library(raster)

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
dt            <- data.frame(id = row.names(pols))
row.names(dt) <- row.names(pols)
pols          <- SpatialPolygonsDataFrame(data = dt, pols)
projection(pols) <- katproj

rgdal::writeOGR(pols, dsn = "inst/extdata/kattegat_coast.gpkg", 
								layer = "pol", driver = "GPKG", overwrite_layer = TRUE)

pnts <- data.frame(id = seq_len(nrow(kattegat$coords)), salinity = kattegat$data)
pnts <- cbind(pnts, kattegat$coords)
coordinates(pnts) <- ~ x.utm + y.utm
projection(pnts)  <- katproj
rgdal::writeOGR(pnts, dsn = "inst/extdata/kattegat_pnts.gpkg", 
								layer = "pnts", driver = "GPKG", overwrite_layer = TRUE)	
