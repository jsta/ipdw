context("errorGen")

test_that("errorGen works", {
  library(sf)
  validation.data <- data.frame(rnorm(10, mean = 0.2, sd = 1))
  names(validation.data) <- c("validation")
  validation.sf_ob <- validation.data
  validation.data <- as.numeric(unlist(validation.data))
  xy <- data.frame(x = c(0:9), y = rep(1, 10))
  validation.sf_ob <- st_as_sf(cbind(validation.sf_ob, xy), coords = c("x", "y"))

  m <- matrix(1, 1, 10)
  out.ras <- raster::raster(m, xmn = 0, xmx = ncol(m), ymn = 0, ymx = nrow(m))
  out.ras[] <- validation.data + rnorm(ncell(out.ras), mean = 0.01,
    sd = 0.2)
  valid.stats <- errorGen(out.ras, validation.sf_ob, validation.data,
    plot = FALSE, title = "Validation Plot")
  expect_equal(length(valid.stats), 2)

})
