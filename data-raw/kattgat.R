library(sf)
library(geoR)

data(kattegat)
katproj <- c("+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defs")

pols <- st_sf(geometry = st_sfc(lapply(kattegat$dk, function(x) st_polygon(list(x))), crs = katproj))
st_write(pols, "inst/extdata/kattegat_coast.gpkg", layer = "pol", append = FALSE)

pnts <- data.frame(id = seq_len(nrow(kattegat$coords)), salinity = kattegat$data)
pnts <- cbind(pnts, kattegat$coords)
pnts <- st_as_sf(pnts, coords = c("x.utm", "y.utm"), crs = katproj)
st_write(pnts, "inst/extdata/kattegat_pnts.gpkg", layer = "pnts", append = FALSE)
