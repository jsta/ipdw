context("pathdistGen")

test_that("pathdistGen works", {
  expect_error(pathdistGen(1, raster::raster(1), 1, progressbar = FALSE),
  						 "sf_ob object must be of class sf")
  
})
