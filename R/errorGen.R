#' @name errorGen
#'
#' @title Generate interpolation error stats from validation datasets
#' @description Generate error statistics from validation point datasets overlaid on a raster surface
#'
#' @param finalraster RasterLayer object
#' @param validation.sf_ob sf object with points geometry
#' @param validation.data data.frame
#' @param plot logical. Plot comparison?
#' @param title Plot labels
#'
#' @return List of error statistics
#'
#' @export
#'
#' @examples
#' library(sf)
#' validation.data <- data.frame(rnorm(10, mean = 0.2, sd = 1))
#' names(validation.data) <- c("validation")
#' validation.sf_ob <- validation.data
#' validation.data <- as.numeric(unlist(validation.data))
#' xy <- data.frame(x = c(0:9), y = rep(1, 10))
#' validation.sf_ob <- st_as_sf(cbind(validation.sf_ob, xy), coords = c("x", "y"))
#'
#' m <- matrix(NA, 1, 10)
#' out.ras <- raster(m, xmn = 0, xmx = ncol(m), ymn = 0, ymx = nrow(m))
#' out.ras[] <- validation.data + rnorm(ncell(out.ras), mean = 0.01, sd = 0.2)
#'
#' valid.stats <- errorGen(out.ras, validation.sf_ob, validation.data, plot = TRUE,
#'   title = "Validation Plot")
#' valid.stats
errorGen <- function(finalraster, validation.sf_ob, validation.data,
                     plot = FALSE, title = "") {

  predicted <- raster::extract(finalraster, validation.sf_ob)

  mvp    <- data.frame(cbind(validation.data, predicted))
  fit    <- stats::lm(mvp[, 2] ~ mvp[, 1])
  r2     <- round(summary(fit)$r.squared, 2)
  rmse   <- round(sqrt(mean((mvp[, 1] - mvp[, 2])^2, na.rm = TRUE)), 2)
  logmse <- round(log(mean((mvp[, 1] - mvp[, 2])^2, na.rm = TRUE)), 2)
  pe    <- round(100 * mean(mvp[, 1] / mvp[, 2], na.rm = TRUE), 2)
  stats <- data.frame(r2, rmse, pe, logmse)
  names(stats) <- c("r2", "rmse", "pe", "logmse")

  stats <- list(stats, mvp)

  # optional plotting
  if (plot == TRUE) {
    plot(mvp[, 1], mvp[, 2], ylim = range(mvp[, 2], na.rm = TRUE),
      xlim = range(mvp[, 1], na.rm = TRUE), ylab = "Interpolated",
      xlab = "Measured", main = title)
    graphics::abline(fit, col = "red")
    graphics::abline(a = 0, b = 1)
  }

  return(stats)
}