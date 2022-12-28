#' @name ipdwInterp
#'
#' @title Inverse Distance Weighting with custom distances
#' @description This function takes a rasterstack of pathdistances and generates surfaces by weighting parameter values by these distances
#' @param sf_ob sf object with point geometries
#' @param rstack RasterStack of path distances
#' @param paramlist character. String representing parameter names
#' @param overlapped logical. Default is FALSE, specify TRUE if some points lie on top of barriers
#' @param yearmon character. String specifying the name of the sf object
#' @param removefile logical. Remove files after processing?
#' @param dist_power numeric. Distance decay power (p)
#' @param trim_rstack logical. Trim the raster stack by the convex hull of sf_ob
#'
#' @details Under the hood, this function evaluates:
#' \deqn{V = \frac{\sum\limits_{i=1}^n v_i \frac{1}{d_i^p}}{\sum\limits_{i=1}^n \frac{1}{d_i^p}}}
#' where `d` is the distance between prediction and measurement points,
#' `v_i` is the measured parameter value, and `p` is a power parameter.
#'
#' @return RasterLayer
#'
#' @importFrom raster calc reclassify writeRaster stack rasterize cover
#' @importFrom methods new slot
#' @importFrom sf st_as_sf st_convex_hull
#' @export
#'
#' @examples
#' library(sf)
#' sf_ob <- data.frame(rnorm(2))
#' xy    <- data.frame(x = c(4, 2), y = c(8, 4))
#' sf_ob <- st_as_sf(cbind(sf_ob, xy), coords = c("x", "y"))
#'
#' m <- matrix(NA, 10, 10)
#' costras <- raster(m, xmn = 0, xmx = ncol(m), ymn = 0, ymx = nrow(m))
#'
#' # introduce spatial gradient
#' costras[] <- runif(ncell(costras), min = 1, max = 10)
#' for (i in 1:nrow(costras)) {
#'   costras[i, ] <- costras[i, ] + i
#'   costras[, i] <- costras[, i] + i
#' }
#'
#' rstack <- pathdistGen(sf_ob, costras, 100, progressbar = FALSE)
#' final.raster <- ipdwInterp(sf_ob, rstack, paramlist = c("rnorm.2."), overlapped = TRUE)
#' plot(final.raster)
#' plot(sf_ob, add = TRUE)
ipdwInterp <- function(sf_ob, rstack, paramlist, overlapped = FALSE,
                       yearmon = "default", removefile = TRUE, dist_power = 1,
                       trim_rstack = FALSE) {

  if (missing(paramlist)) {
    stop("Must pass a specific column name to the paramlist argument.")
  }

  if (any(!(paramlist %in% names(sf_ob)))) {
    stop(
      paste0("Variable(s) '",
        paste0(paramlist[!(paramlist %in% names(sf_ob))],
          collapse = "', '"), "' does not exist in sf_ob object."))
  }


  range <- slot(rstack, "range")

  if (trim_rstack) {
    rstack <- raster::mask(rstack, sf::st_convex_hull(sf_ob), inverse = FALSE)
  }

  for (k in seq_len(length(paramlist))) {
    points_layers <- rm_na_pointslayers(param_name = paramlist[k],
      sf_ob = sf_ob, rstack = rstack)
    sf_ob   <- points_layers$sf_ob
    rstack <- points_layers$rstack

    # if(overlapped == TRUE){ #need to set na.rm = TRUE if points are on land?
    rstack.sum <- raster::calc(rstack, fun = function(x) {
      sum(x^dist_power, na.rm = TRUE)
    })
    rstack.sum <- raster::reclassify(rstack.sum, cbind(0, NA))

    # calculate the weight of the individual rasters
    for (i in 1:dim(rstack)[3]) {
      # add power in numerator of below line
      ras.weight   <- rstack[[i]]^dist_power / rstack.sum
      param.value  <- data.frame(sf_ob[i, paramlist[k]])
      param.value2 <- as.vector(unlist(param.value[1]))
      ras.mult     <- ras.weight * param.value2

      rf <- raster::writeRaster(ras.mult,
        filename = file.path(tempdir(), paste(paramlist[k],
          "A5ras", i, ".grd", sep = "")), overwrite = TRUE)
    }

    raster_data_full <- list.files(path = file.path(tempdir()),
      pattern = paste(paramlist[k], "A5ras*", sep = ""),
      full.names = TRUE)
    raster_data <- raster_data_full[grep(".grd", raster_data_full,
      fixed = TRUE)]
    as.numeric(gsub(".*A5ras([0123456789]*)\\.grd$", "\\1",
      raster_data)) -> fileNum
    raster_data <- raster_data[order(fileNum)]

    # sum rasters to get final surface
    rstack.mult <- raster::stack(raster_data)
    finalraster <- raster::calc(rstack.mult, fun = function(x) {
      sum(x, na.rm = TRUE)
    })

    if (overlapped == TRUE) {
      finalraster <- raster::reclassify(finalraster, cbind(0, NA))
    }

    r           <- raster::rasterize(sf_ob, rstack[[1]], paramlist[k])
    finalraster <- raster::cover(r, finalraster)
    finalraster <- new("ipdwResult", finalraster,
      range = range,
      dist_power = dist_power)

    file.remove(raster_data_full)
    return(finalraster)
  }
  # optional removal of path distances

  if (removefile == TRUE) {
    file.remove(list.files(path = file.path(tempdir()),
      pattern = paste(yearmon, "A4ras*", sep = "")))
    file.remove(list.files(path = file.path(tempdir()),
      pattern = paste(paramlist[k], "A5ras*", sep = ""), full.names = TRUE))
  }
}

#' @name rm_na_pointslayers
#'
#' @title Remove NA points features and drop corresponding raster stack layers
#' @description Remove NA points features and drop corresponding raster stack layers
#' @param param_name character name of data column
#' @param sf_ob sf object with point geometries
#' @param rstack RasterStack or RasterBrick
#' @export

rm_na_pointslayers <- function(param_name, sf_ob, rstack) {
  param_index_x <- which(names(sf_ob) == param_name)  
  param_na_y    <- which(is.na(sf_ob[, param_index_x]))

  if (length(param_na_y) > 0) {
    sf_ob   <- sf_ob[-which(is.na(sf_ob[, param_index_x])), ]
    rstack <- raster::dropLayer(rstack, param_na_y)
  }
  list(sf_ob = sf_ob, rstack = rstack)
}

ipdwResult <- setClass("ipdwResult",
  slots = c(range = "numeric", dist_power = "numeric"),
  contains = "RasterLayer")
