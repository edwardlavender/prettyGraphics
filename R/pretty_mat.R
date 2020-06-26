#######################################
#######################################
#### pretty_mat()

#' @title Plot pretty matrices
#' @description This function is a wrapper for \code{\link[fields]{image.plot}} designed to streamline the visualisation of pretty matrices. The function returns a plot which can be customised.
#'
#' @param z A matrix to be plotted. Axis tick mark labels are extracted from the row and column names of the matrix, if provided. Otherwise, axis tick mark labels are given as an index.
#' @param zlim A numeric vector of length two which specifies the z axis limits (see \code{\link[fields]{image.plot}}),
#' @param col A colour table to use for the image (see \code{\link[fields]{image.plot}}).
#' @param col_diag (optional) A colour which, if provided, is used to shade the diagonal of the matrix. This is useful for symmetric square matrices.
#' @param grid (optional) A named list which, if provided, is passed to \code{\link[prettyGraphics]{add_grid_rect_xy}} to draw grid lines around each matrix cell. Arguments \code{x} and \code{y} default to the positions of each matrix cell and do not need to be provided. To use \code{\link[prettyGraphics]{add_grid_rect_xy}}'s default graphical options, simply specify \code{grid = list()}.
#' @param cex.axis A number which specifies the axis font size (specifically, the magnification to be used for axis annotation relative to the current setting of \code{cex}, see \code{\link[graphics]{par}}).
#' @param las The style of axis labels (see \code{\link[graphics]{par}}).
#' @param xlab The x axis label.
#' @param ylab The y axis label.
#' @param ... Additional arguments passed to \code{\link[fields]{image.plot}}. These should not include \code{x}, \code{y}, \code{xlim}, \code{ylim} or \code{axes} which are controlled internally.
#'
#' @return The function returns a plot of a matrix with coloured cells.
#'
#' @examples
#' #### Define an example square matrix
#' n <- 25
#' mat <- matrix(sample(0:100, size = n^2, replace = TRUE), ncol = 25, nrow = 25)
#'
#' #### Example (1): The default options
#' pretty_mat(mat)
#'
#' #### Example (2): Customisation
#' # Adjust labelling
#' rownames(mat) <- colnames(mat) <- LETTERS[1:nrow(mat)]
#' pretty_mat(mat)
#' pretty_mat(mat, xlab = "ID", ylab = "ID")
#' # Adjust z limits
#' pretty_mat(mat, zlim = c(0, 200))
#' # Adjust colour scheme
#' pretty_mat(mat, col = grey.colors(100))
#' # Colour the diagonal (useful for symmetric square matrices)
#' pretty_mat(mat, col_diag = "black")
#' # Add a grid using default options
#' pretty_mat(mat, grid = list())
#' # Customise the grid
#' pretty_mat(mat, grid = list(col = "black", lty = 1))
#'
#' #### Example (3) Rectangular matrices function similarly
#' # Define matrix
#' n1 <- 5; n2 <- 10
#' mat <- matrix(sample(0:100, size = n1*n2, replace = TRUE), ncol = n1, nrow = n2)
#' utils::str(mat)
#' # Visualise matrix with default options
#' pretty_mat(mat)
#' # Adjust names
#' rownames(mat) <- LETTERS[1:nrow(mat)]
#' colnames(mat) <- LETTERS[1:ncol(mat)]
#' pretty_mat(mat)
#' # Other options can be implemented as above
#' pretty_mat(mat, grid = list(lty = 1))
#' # However, adding colours along the diagonal may not make sense for asymmetric matrices
#' pretty_mat(mat, col_diag = "grey")
#'
#' @author Edward Lavender
#' @export
#'

pretty_mat <-
  function(z,
           zlim = NULL,
           col = grDevices::colorRampPalette(c("white", "yellow", "orange", "red"))(100),
           col_diag = NULL,
           grid = NULL,
           cex.axis = 1,
           las = TRUE,
           xlab = "Individual", ylab = "Individual",...
  ){

    #### Checks
    ## Check that no arguments that are set internally have been supplied via ...
    not_allowed <- c("x", "y", "xlim", "ylim", "axes")
    check...(c("xlim", "ylim"),...)

    #### Plot image, note the need to shift limits by 0.5 down:
    nx <- nrow(z)
    ny <- ncol(z)
    if(is.null(rownames(z))) x_names <- 1:nx else x_names <- rownames(z)
    if(is.null(colnames(z))) y_names <- 1:ny else y_names <- colnames(z)
    fields::image.plot(1:nx, 1:ny, z,
                       col = col,
                       xlim = c(0.5, nx+0.5), ylim = c(0.5, ny+0.5), zlim = zlim,
                       xlab = xlab, ylab = ylab,
                       axes = FALSE,...)

    #### Add regular grid for interpretation, if requested
    if(!is.null(grid)){
      if(!is.list(grid)) stop("'grid', if supplied, must be a named list.")
      if(length(grid) > 0) if(is.null(names(grid))) stop("'grid', if supplied, must be a named list (names are missing).")
      grid <- list_merge(list(x = 0.5:(nx+0.5), y = 0.5:(ny+0.5)), grid)
      do.call(add_grid_rect_xy, grid)
    }

    #### Add axes
    xat <- 0.5:(nx+0.5)
    yat <- 0.5:(ny+0.5)
    graphics::axis(side = 1, at = xat, c(x_names, ""), pos = 0.5, cex.axis = cex.axis, las = las)
    graphics::axis(side = 2, at = yat, c(y_names, ""), pos = 0.5, cex.axis = cex.axis, las = las)
    graphics::axis(side = 3, at = xat, lwd.ticks = 0, labels = FALSE)
    graphics::axis(side = 4, at = yat, lwd.ticks = 0, labels = FALSE)

    #### Shade diagonal for interpretion
    if(!is.null(col_diag)){
      zdiag <- z; zdiag[] <- NA; diag(zdiag) <- 1
      graphics::image(1:nx, 1:ny, zdiag, add = TRUE, col = col_diag)
    }

  }
