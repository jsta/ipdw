#' @name costrasterGen
#'
#' @title Generate a cost Raster
#' @description Generate a cost raster from an object of class sf with point or polygon geometries
#'
#' @param xymat Matrix of coordinates or an sf object with point geometries
#' @param pols sf object with polygon geometries
#' @param extent Define extent based on extent of xymat/sf (points) or pols (polys). Default is polys.
#' @param resolution Numeric defaults to 1. See \code{\link[raster]{raster}}.
#' @param projstr proj4 string defining the output projection. A warning will be thrown if projstr does not match the projection of the extent target. Pass NULL for non-geographic grids.
#'
#' @details Ensure that the projection of the xymat coordinates and pols match. If they do not match use the \code{st_transform} command.
#' @seealso \code{\link[raster]{rasterize}}
#'
#' @return RasterLayer
#'
#' @importFrom raster raster rasterize reclassify
#' @importFrom sf st_polygon st_as_sf st_crs st_bbox
#' @importFrom methods is
#' @export
#'
#' @examples \dontrun{
#' library(sf)
#' Sr1 <- st_polygon(list(cbind(c(0, 0, 1, 1, 0), c(0, 12, 12, 0, 0))))
#' Sr4 <- st_polygon(list(cbind(c(9, 9, 10, 10, 9), c(0, 12, 12, 0, 0))))
#' Sr2 <- st_polygon(list(cbind(c(1, 1, 9, 9, 1), c(11, 12, 12, 11, 11))))
#' Sr3 <- st_polygon(list(cbind(c(1, 1, 9, 9, 1), c(0, 1, 1, 0, 0))))
#' Sr5 <- st_polygon(list(cbind(c(4, 4, 5, 5, 4), c(4, 8, 8, 4, 4))))
#' pols <- st_as_sf(st_sfc(Sr1, Sr2, Sr3, Sr4, Sr5,
#'   crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
#'
#' # using a matrix object
#' xymat <- matrix(3, 3, nrow = 1, ncol = 2)
#' costras <- costrasterGen(xymat, pols, projstr = NULL)
#'
#' # plotting
#' plot(costras)
#' points(xymat)
#' }
costrasterGen <- function(xymat, pols, extent = "polys", projstr,
                          resolution = 1) {

  # add check to see if projstr and projection(pols) match
  if (!identical(projstr, st_crs(pols)$proj4string)) {
    message("Warning, the projection of polygons does not
    				match projstr. See sf::st_transform")
  }

  # define spatial domain based on pnts or polys
  if (extent == "polys" | extent == "pols") {
    xmn <- as.numeric(st_bbox(pols)["xmin"])
    xmx <- as.numeric(st_bbox(pols)["xmax"])
    ymn <- as.numeric(st_bbox(pols)["ymin"])
    ymx <- as.numeric(st_bbox(pols)["ymax"])
  }

  if (extent == "points" | extent == "pnts") {
    if(is(xymat, "sf")) {
      xmn <- as.numeric(st_bbox(xymat)["xmin"])
      xmx <- as.numeric(st_bbox(xymat)["xmax"])
      ymn <- as.numeric(st_bbox(xymat)["ymin"])
      ymx <- as.numeric(st_bbox(xymat)["ymax"])
    }else {
      ymn <- range(xymat[, 2])[1]
      ymx <- range(xymat[, 2])[2]
      xmn <- range(xymat[, 1])[1]
      xmx <- range(xymat[, 1])[2]
    }
  }

  nrow <- ymx - ymn
  ncol <- xmx - xmn

  # generate cost raster
  if (resolution != 1) {
    r <- raster::raster(nrow = nrow, ncol = ncol, crs = projstr, xmn = xmn,
      xmx = xmx, ymn = ymn, ymx = ymx, resolution = resolution)
  } else {
    r <- raster::raster(nrow = nrow, ncol = ncol, crs = projstr, xmn = xmn,
      xmx = xmx, ymn = ymn, ymx = ymx)
  }

  costras <- raster::rasterize(pols, r, silent = TRUE)
  m <- c(0, +Inf, 10000)
  rclmat <- matrix(m, ncol = 3, byrow = TRUE)
  costras <- raster::reclassify(costras, rclmat)
  costras <- raster::reclassify(costras, cbind(NA, 1))

  return(costras)
}
