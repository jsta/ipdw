context("ipdwInterp")

test_that("ipdwInterp works", {
  sf_ob                 <- data.frame(aa = rnorm(2))
  xy                    <- data.frame(x = c(4, 2), y = c(8, 4))
  sf_ob                 <- st_as_sf(cbind(sf_ob, xy), coords = c("x", "y"))
  m                     <- matrix(NA, 10, 5)
  costras               <- raster::raster(xmn = 0, xmx = ncol(m),
    ymn = 0, ymx = nrow(m))
  projection(costras)   <- NA
  costras[]             <- 1

  costras[]     <- runif(raster::ncell(costras), min = 1, max = 10)
  for (i in seq_len(nrow(costras))) {
    costras[i, ] <- costras[i, ] + i
  }
  for (i in seq_len(ncol(costras))) {
    costras[, i]  <- costras[, i] + i
  }

  rstack       <- pathdistGen(sf_ob, costras, 100, progressbar = FALSE)
  pointslayers <- rm_na_pointslayers(param_name = "rnorm.2.", sf_ob = sf_ob,
    rstack = rstack)
  rstack       <- pointslayers$rstack
  sf_ob        <- pointslayers$sf_ob
  res          <- ipdwInterp(sf_ob, rstack, "aa")

  expect_identical(dim(rstack)[3], nrow(sf_ob))
  expect_s4_class(res, "RasterLayer")
  expect_error(ipdwInterp(sf_ob, rstack),
    "Must pass a specific column name to the paramlist argument.")

  expect_error(ipdwInterp(sf_ob, rstack, "bb"),
    "Variable(s) 'bb' does not exist in sf_ob object.", fixed = TRUE)

  expect_s4_class(ipdwInterp(sf_ob, rstack, "aa", trim_rstack = TRUE), "RasterLayer")
})

# test_that("raster should not given warning", {
# 	skip_on_cran()
# 	skip_on_travis()
# 	skip_on_appveyor()
#
# 	m <- matrix(NA, 10, 10)
# 	expect_s4_class(raster::raster(xmn = 0, xmx = 10, ymn = 0, ymx = 10),
#   "RasterLayer")
# })
