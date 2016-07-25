context("ipdwInterp")

test_that("ipdwInterp works", {
	skip_on_cran()
	skip_on_travis()
	skip_on_appveyor()
  
	spdf <- data.frame(rnorm(2))
	xy <- data.frame(x = c(4, 2), y = c(8, 4))
	coordinates(spdf) <- xy
	m <- matrix(NA, 10, 10)
	costras <- raster(xmn = 0, xmx = ncol(m), ymn = 0, ymx = nrow(m))
	costras[] <- NA
	
	costras[] <- runif(ncell(costras), min = 1, max = 10)
	for(i in seq_len(nrow(costras))){
	  costras[i,] <- costras[i,] + i
	  costras[,i] <- costras[,i] + i
	}
	
	rstack <- pathdistGen(spdf, costras, 100, progressbar = FALSE)
	
	pointslayers <- rm_na_pointslayers(param_name = "rnorm.2.", spdf = spdf,
									rstack = rstack)
	
	rstack <- pointslayers$rstack
	spdf <- pointslayers$spdf
	
	expect_identical(dim(rstack)[3], nrow(spdf))
  
})

# test_that("raster should not given warning", {
# 	skip_on_cran()
# 	skip_on_travis()
# 	skip_on_appveyor()
# 	
# 	m <- matrix(NA, 10, 10)
# 	browser()
# 	expect_s4_class(raster::raster(xmn = 0, xmx = 10, ymn = 0, ymx = 10),
#   "RasterLayer")
# })
