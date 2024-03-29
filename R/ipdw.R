#' @name ipdw
#'
#' @title Inverse Path Distance Weighting
#' @description Interpolate geo-referenced point data using inverse path distance weighting.
#'
#' @param sf_ob sf object with point geometries
#' @param costras RasterLayer. Cost raster
#' @param range numeric. Range of interpolation neighborhood
#' @param paramlist character. String representing parameter names
#' @param overlapped logical. Default is FALSE, specify TRUE if some points lie on top of barriers
#' @param yearmon character. String specifying the name of the sf_ob
#' @param removefile logical. Remove files after processing?
#' @param step numeric. Number of sub loops to manage memory during raster processing.
#' @param dist_power numeric. Distance decay power (p)
#' @param trim_rstack logical. Trim the raster output by the convex hill of sf_ob
#'
#' @return RasterLayer
#'
#' @details This is a high level function that interpolates an
#' sf object with point geometries in a single pass.
#'
#' Points must be located within a single contiguous area. The presence of "landlocked"
#' points will cause errors. It may be necessary to increase the value
#' assigned to land areas when using a large range value in combination
#' with a large sized cost rasters (grain x extent). In these cases, the
#' value of land areas should be increased to ensure that it is always
#' greater than the maximum accumulated cost path distance of any given geo-referenced point.
#'
#' @export
#' @importFrom raster projection
#'
#' @examples
#' # see vignette
ipdw <- function(sf_ob, costras, range, paramlist, overlapped = FALSE,
                 yearmon = "default", removefile = TRUE, step = 16, dist_power = 1,
                 trim_rstack = FALSE) {

  pathdists <- pathdistGen(sf_ob, costras, range, step)

  final.ipdw <- ipdwInterp(sf_ob, pathdists, paramlist, yearmon,
    removefile = TRUE, overlapped = overlapped,
    dist_power = dist_power, trim_rstack = trim_rstack)

  return(final.ipdw)
}
