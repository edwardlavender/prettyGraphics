#######################################
#######################################
#### pretty_mat()

#' @title Plot pretty matrices
#' @description This function is a wrapper for \code{\link[fields]{image.plot}} designed to streamline the visualisation of pretty matrices. The function returns a plot which can be customised.
#'
#' @param z A matrix to be plotted.
#' @param retain_orientation A logical input which defines whether or not the plot should retain the same orientation as the printed matrix. The default is \code{FALSE}, in which case the underlying plotting function (\code{\link[fields]{image.plot}}) rotates the matrix by 90 degrees counter-clockwise in the plot. In contrast, \code{retain_orientation = TRUE} forces the matrix and the plot to retain the same orientation (see Details).
#' @param zlim A numeric vector of length two which specifies the z axis limits (see \code{\link[fields]{image.plot}}),
#' @param col A colour table to use for the image (see \code{\link[fields]{image.plot}}).
#' @param col_diag (optional) A colour which, if provided, is used to shade the diagonal of the matrix. This is useful for symmetric square matrices.
#' @param grid (optional) A named list which, if provided, is passed to \code{\link[prettyGraphics]{add_grid_rect_xy}} to draw grid lines around each matrix cell. Arguments \code{x} and \code{y} default to the positions of each matrix cell and do not need to be provided. To use \code{\link[prettyGraphics]{add_grid_rect_xy}}'s default graphical options, simply specify \code{grid = list()}.
#' @param add_axes A logical input which defines whether or not to add axes to the plot. Axes tick marks are given every nth column and every nth row (see below).
#' @param xtick_every_n A numeric input which defines the spacing between sequential tick marks for the x axis. Tick marks are given every \code{xtick_every_n} (see Details).
#' @param ytick_every_n A numeric input which defines the spacing between sequential tick marks for the other axis. Tick marks are given every \code{ytick_every_n} (see Details).
#' @param cex.axis A number which specifies the axis font size (see \code{\link[graphics]{par}}).
#' @param las The style of axis labels (see \code{\link[graphics]{par}}).
#' @param xlab The x axis label.
#' @param ylab The y axis label.
#' @param ... Additional arguments passed to \code{\link[fields]{image.plot}}. These should not include \code{x}, \code{y}, \code{xlim}, \code{ylim} or \code{axes} which are controlled internally.
#'
#' @details The limits of the plot are set between (0.5, number of rows + 0.5) and (0.5, number of cols + 0.5). Axes are added by default, but can be suppressed. If available, matrix row/column names are used to define axis labels. Otherwise, an index is used. Axes labels are added every \code{xtick_every_n} and \code{ytick_every_n}. If \code{retain_orientation} is \code{FALSE}, the plot is rotated 90 degrees counter-clockwise relative to the input matrix and thus read from the bottom-left to the top-right (like a map), so that matrix columns and corresponding tick mark labels are represented along the y axis whereas matrix rows and corresponding tick mark labels are represented along the x axis. In contrast, if \code{retain_orientation} is \code{TRUE}, the plot is read from the top-left to bottom-right.
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
#' # Adjust labelling and note how under the default settings the plot
#' # ... is rotated by 90 degrees counter-clockwise
#' rownames(mat) <- colnames(mat) <- LETTERS[1:nrow(mat)]
#' pretty_mat(mat)
#' pretty_mat(mat, xlab = "ID", ylab = "ID")
#' # Adjust the number of labels
#' pretty_mat(mat, xlab = "ID", ylab = "ID",
#'            xtick_every_n = 5, ytick_every_n = 2)
#' # Compare to plot with retained orientation
#' pretty_mat(mat, xlab = "ID", ylab = "ID",
#'            xtick_every_n = 5, ytick_every_n = 2,
#'            retain_orientation = TRUE)
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
#' #' #### Example (4): Understanding the retain_orientation argument:
#' # ... Under the default option (retain_orientation is FALSE), R plots a
#' # ... 90 degree counter-clockwise rotation of the conventional
#' # ... printed layout of a matrix. This can be difficult to interpret.
#' # ... retain_orientation = TRUE ensures the matrix is plotted in the
#' # ... correct orientation
#' # Define a simple matrix
#' n <- 3
#' mat <- matrix(1:9, ncol = 3, nrow = 3)
#' # Distinguish row names from column names
#' rownames(mat) <- LETTERS[1:ncol(mat)]
#' # Check matrix
#' mat
#' # Visualise 'default' versus 'expected' matrix
#' pp <- par(mfrow = c(1, 2))
#' pretty_mat(mat, retain_orientation = FALSE, main = "default (rotated)")
#' pretty_mat(mat, retain_orientation = TRUE, main = "custom (original orientation)")
#' par(pp)
#'
#'
#' @author Edward Lavender
#' @export
#'

pretty_mat <-
  function(z,
           retain_orientation = FALSE,
           zlim = NULL,
           col = grDevices::colorRampPalette(c("white", "yellow", "orange", "red"))(100),
           col_diag = NULL,
           grid = NULL,
           add_axes = TRUE,
           xtick_every_n = 1, ytick_every_n = xtick_every_n,
           cex.axis = 1,
           las = TRUE,
           xlab = "", ylab = "",...
  ){

    #### Checks
    ## Check that no arguments that are set internally have been supplied via ...
    not_allowed <- c("x", "y", "xlim", "ylim", "axes")
    check...(c("xlim", "ylim"),...)

    #### Force appropriate orientation on plot
    # .. This is necessary because image.plot rotates the plot by 90 degrees
    # ... counter-clockwise relative to the printed matrix.
    if(retain_orientation){
      z <- apply(z, 2, rev)
      z <- t(z)
    }

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

    #### Add a box around the plot
    xat <- 0.5:(nx+0.5)
    yat <- 0.5:(ny+0.5)
    graphics::axis(side = 1, at = xat, lwd.ticks = 0, labels = FALSE)
    graphics::axis(side = 2, at = yat, lwd.ticks = 0, labels = FALSE)
    graphics::axis(side = 3, at = xat, lwd.ticks = 0, labels = FALSE)
    graphics::axis(side = 4, at = yat, lwd.ticks = 0, labels = FALSE)

    #### Add axes
    if(add_axes){
      ## Axis param
      xat <- xat[seq(1, length(xat), by = xtick_every_n)]
      x_names <- c(x_names, "")
      x_names <- x_names[seq(1, length(x_names), by = xtick_every_n)]
      if(!retain_orientation){
        yat <- yat[seq(1, length(yat), by = ytick_every_n)]
        y_names <- c(y_names, "")
        y_names <- y_names[seq(1, length(y_names), by = ytick_every_n)]
      } else{
        y_names <- c("", y_names)
        yat <- yat[seq(length(yat), 1, by = -ytick_every_n)]
        y_names <- y_names[seq(length(y_names), 1, by = -ytick_every_n)]
      }

      ## Add axes
      graphics::axis(side = 1, at = xat, x_names, pos = 0.5, cex.axis = cex.axis, las = las)
      graphics::axis(side = 2, at = yat, y_names, pos = 0.5, cex.axis = cex.axis, las = las)
    }

    #### Shade diagonal for interpretion
    if(!is.null(col_diag)){
      zdiag <- z; zdiag[] <- NA; diag(zdiag) <- 1
      graphics::image(1:nx, 1:ny, zdiag, add = TRUE, col = col_diag)
    }

  }
