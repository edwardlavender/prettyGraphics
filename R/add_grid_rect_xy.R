#' @title Add a regular, rectangular grid to a plot at user-specified positions
#' @description This function adds a regular grid to a plot at positions specified by \code{x} and \code{y} vectors. This function differs from \code{\link[graphics]{grid}} in which the user can specify the number of cells in each direction, but not their positions.
#'
#' @param x The x coordinates of the grid lines.
#' @param y The y coordinates of the grid lines.
#' @param col A numeric or character input specifying the colour of the grid lines.
#' @param lwd A number specifying the thickness of the grid lines.
#' @param lty A numeric or character input specifying the line type.
#' @param ... Other graphical parameters passed to \code{\link[graphics]{arrows}} which is used to draw grid lines, except \code{length} which is set internally.
#'
#' @return The function adds a grid to a plot.
#'
#' @examples
#' #### Example (1)
#' # Plot some example data and hide axes for neatness
#' set.seed(1)
#' plot(runif(100, 0, 10), runif(100, 5, 25), xlim = c(0, 10), ylim = c(5, 25), axes = FALSE)
#' # Add grid at user-specified positions to aid interpretation
#' add_grid_rect_xy(seq(0, 10, by = 2), seq(5, 25, by = 5))
#' # Add axes at the end for neatness
#' axis(side = 1, seq(0, 10, by = 2), pos = 5); axis(side = 2, seq(5, 25, by = 5), pos = 0)
#'
#' @author Edward Lavender
#' @export
#'

##############################################
##############################################
#### add_grid_rect_xy()

add_grid_rect_xy <-
  function(x,
           y,
           col = "lightgrey",
           lwd = 1,
           lty = 3,...){

    # Define ranges
    rx <- range(x)
    ry <- range(y)

    # Define horizontal components using arrows (vectorised):
    graphics::arrows(x0 = rx[1], x1 = rx[2], y0 = y,
                     length = 0,
                     col = col, lwd = lwd, lty = lty,...)

    # Define vertical components
    graphics::arrows(x0 = x, y0 = ry[1], y1 = ry[2],
                     length = 0,
                     col = col, lwd = lwd, lty = lty,...)

  } # close function


#### End of code.
##############################################
##############################################
