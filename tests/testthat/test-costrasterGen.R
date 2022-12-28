context("costrasterGen")

test_that("returns correct class and warn on bad projection", {
  library(sf)

  latlonproj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  Sr1 <- st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 12, 12, 0, 0))))
  Sr4 <- st_polygon(list(cbind(c(9, 9, 10, 10, 9), c(0, 12, 12, 0, 0))))
  Sr2 <- st_polygon(list(cbind(c(1, 1, 9, 9, 1), c(11, 12, 12, 11, 11))))
  Sr3 <- st_polygon(list(cbind(c(1, 1, 9, 9, 1), c(0, 1, 1, 0, 0))))
  Sr5 <- st_polygon(list(cbind(c(4, 4, 5, 5, 4), c(4, 8, 8, 4, 4))))
  pols <- st_as_sf(st_sfc(Sr1, Sr2, Sr3, Sr4, Sr5,
    crs = latlonproj))

  xymat <- matrix(3, 3, nrow = 1, ncol = 2)

  expect_is(costrasterGen(xymat, pols, projstr = latlonproj), "RasterLayer")
  expect_message(costrasterGen(xymat, pols, projstr = NULL),
    "Warning, the projection of polygons does not
    				match projstr. See sf::st_transform")

})

test_that("SpatialPoints objects are handled correctly", {
  library(sf)
  pols <- st_read(system.file("extdata/kattegat_coast.gpkg", package = "ipdw"), quiet = TRUE)
  pnts <- st_read(system.file("extdata/kattegat_pnts.gpkg", package = "ipdw"), quiet = TRUE)
  costras <- costrasterGen(pnts, pols, extent = "pnts",
    projstr = projection(pols))
  expect_is(costras, "RasterLayer")
})
