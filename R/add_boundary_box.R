#' @title Add a bounding box around observations at specified coordinates
#' @description This function adds a bounding box around observations on a plot at specified coordinates.
#' @param x A vector of length four which specifies (1) the minimum x value of the box; (2) the maximum x value of the box; (3) the minimum y value of the box; and (4) the maximum y value of the box.
#' @param ... Other arguments passed to \code{\link[graphics]{rect}}.
#'
#' @return The function adds a rectangular box around observations at specified coordinates.
#'
#' @examples
#' # Simulate some observations
#' x <- runif(100, 0, 1)
#' y <- runif(100, 10, 20)
#'
#' # Visualise observations and a boundary box around the range of the values
#' # ... and the 0.05 0.95 quantiles:
#' plot(x, y)
#' add_boundary_box(c(range(x), range(y)), border = "red")
#' add_boundary_box(c(stats::quantile(x, prob = c(0.05, 0.95)),
#'                    stats::quantile(y, prob = c(0.05, 0.95))),
#'                    border = "red", lty = 3)
#'
#' # Visualise an example raster and add boundary box of extent
#' raster::plot(dat_gebco)
#' add_boundary_box(raster::extent(dat_gebco), border = "royalblue", lwd = 2)
#'
#' @source This is a simple wrapper for \code{\link[graphics]{rect}}.
#' @author Edward Lavender
#' @export
#'

add_boundary_box <- function(x,...){
  graphics::rect(xleft = x[1],
                 ybottom = x[3],
                 xright = x[2],
                 ytop = x[4],...)
}


