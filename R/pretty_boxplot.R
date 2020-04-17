#' @title Pretty boxplots
#' @description This function makes prettier boxplots. Boxplots are created using \code{\link[graphics]{boxplot}} but the axes are controlled via \code{\link[plot.pretty]{pretty_axis}} and an \code{adj} argument. The addition of axis labels is also more flexible because, while these can be implemented via \code{xlab}, \code{ylab} and \code{main} as usual, axis labels can also be controlled using \code{\link[graphics]{mtext}} via \code{mtext_args}.
#'
#' @param x A factor vector.
#' @param y A numeric vector.
#' @param pretty_axis_args A named list of arguments passed to \code{\link[plot.pretty]{pretty_axis}} to control axes.
#' @param adj A number which defines the distance from the first and last box-and-whiskers to the edges of the x axis.
#' @param xlab The x axis label. This can be added via \code{mtext_args} for more control.
#' @param ylab The y axis label. This can be added via \code{mtext_args} for more control.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to add axis labels. A nested list is used to control each axis separately.
#' @param ... Additional arguments passed to \code{\link[graphics]{boxplot}} These should not include \code{xlim} or \code{ylim}, which are implemented via \code{pretty_axis_args}, nor \code{frame.plot} or \code{axes} which are suppressed so that \code{pretty_axis_args} can control axes.
#'
#' @details Note that, unlike \code{\link[graphics]{boxplot}}, formula notation is not implemented.
#'
#' @return The function returns a pretty boxplot.
#'
#' @examples
#'
#' #### Generate example data
#' set.seed(1)
#' dx <- sample(c("M", "F"), size = 100, replace = TRUE, prob = c(0.2, 0.8))
#' dy <- stats::runif(length(dx), 0, 10)
#' d <- data.frame(x = dx, y = dy)
#'
#' #### Example (1): Comparing graphics::boxplot() and plot.pretty::pretty_boxplot()
#' pp <- par(mfrow = c(1, 2))
#' graphics::boxplot(y ~ x, data = d)
#' pretty_boxplot(d$x, d$y)
#' par(pp)
#'
#' ##### Example (2): Unlike graphics::boxplot() formula notation is not supported
#' \dontrun{
#'   pretty_boxplot( d$y ~ d$x)
#' }
#'
#' #### Example (3): Other arguments can be passed via ... to boxplot
#' pp <- graphics::par(mfrow = c(2, 2))
#' graphics::boxplot(d$y ~ d$x, width = c(5, 1))
#' pretty_boxplot(d$x, d$y, data = d, width = c(5, 1))
#' pretty_boxplot(d$x, d$y, data = d, varwidth = TRUE)
#' # However,xlim, ylim arguments are set via pretty_axis_args and should not be supplied.
#' #\dontrun{
#' #  pretty_boxplot(d$x, d$y, xlim = c(1, 2))
#' #}
#'
#' #### Example (4): Axis labels can be added via xlab, ylab and main
#' # ... or via mtext_args for more control
#' pretty_boxplot(d$x, d$y, xlab = "Sex", ylab = "Response")
#' pretty_boxplot(d$x, d$y,
#'                mtext_args = list(list(side = 1, text = "Sex", font = 2, line = 2),
#'                                  list(side = 2, text = "Response", font = 2, line = 2)))
#'
#' #### Example (5): Axes are controlled via pretty_axis_args
#' pp <- par(mfrow = c(1, 2))
#' pretty_boxplot(d$x, d$y, pretty_axis_args = list(side = 1:2,
#'                                                  pretty = list(n = 10),
#'                                                  axis = list(las = TRUE)))
#' # Add a box around the plot:
#' pretty_boxplot(d$x, d$y, pretty_axis_args = list(side = 1:4,
#'                                                  pretty = list(n = 10),
#'                                                  axis = list(list(),
#'                                                              list(las = TRUE),
#'                                                              list(labels = FALSE, lwd.ticks = 0),
#'                                                              list(labels = FALSE, lwd.ticks = 0)))
#' )
#' par(pp)
#'
#' #### Example (6): Examples with more groups
#' pp <- par(mfrow = c(2, 2))
#' invisible(
#'   lapply(c(5, 9, 12, 15), function(n){
#'     set.seed(1)
#'     dx <- factor(sample(1:n, size = 100, replace = TRUE))
#'     dy <- stats::runif(length(dx), 0, 10)
#'     d <- data.frame(x = dx, y = dy)
#'     pretty_boxplot(d$x, d$y, col = "lightgrey")
#'   })
#' )
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

######################################
######################################
#### pretty_boxplot()

pretty_boxplot <-
  function(x, y,
           pretty_axis_args = list(side = 1:2),
           adj = 0.5,
           xlab = "", ylab = "",
           mtext_args = list(),...){

    #### Checks
    # Use check... function to check additionally supplied arguments are allowed
    # (axis limits are not allowed because these are controlled via pretty_axis_args)
    check...(not_allowed = c("xlim", "ylim", "frame.plot", "axes"),...)
    # Check that x is a factor
    if(!inherits(x, "factor")){
      warning("x co-erced to a factor.")
      x <- factor(x)
    }


    #### Define parameters for pretty axis for x axis
    # Number of groups
    ng <- length(levels(x))
    # x a number
    x_num <- as.numeric(x)
    # Add data to pretty_axis_args
    if(is.null(pretty_axis_args$x)) pretty_axis_args$x <- list(x_num, y)
    # Add limits for x axis, while making sure there is an empty list for the y axis
    if(is.null(pretty_axis_args$lim)) pretty_axis_args$lim <- lapply(pretty_axis_args$side, function(x) NULL)
    if(is.null(pretty_axis_args$lim[[1]])){
      xlim <- c(min(x_num) - adj, max(x_num) + adj)
      pretty_axis_args$lim[[1]] <- xlim
      # Copy limits to 3rd axis, if required:
      if(3 %in% pretty_axis_args$side) pretty_axis_args$lim[[3]] <- xlim
    }
    # Add x at positions for x axis, ensuring there is a list for the y axis
    if(is.null(pretty_axis_args$axis)) pretty_axis_args$axis <- lapply(pretty_axis_args$side, function(x) NULL)
    # We need to duplicate any existing list, if only a single list has been provided that will apply to both axes
    if(plotrix::listDepth(pretty_axis_args$axis) == 1) pretty_axis_args$axis <- lapply(pretty_axis_args$side, function(i) pretty_axis_args$axis)
    if(is.null(pretty_axis_args$axis[[1]]$at)) pretty_axis_args$axis[[1]]$at <- c(xlim[1], 1:ng, xlim[2])
    # Adjust labels, so we will only retain labels for the factor levels
    if(is.null(pretty_axis_args$axis[[1]]$labels)) pretty_axis_args$axis[[1]]$labels <- c("", levels(x), "")

    #### Implement pretty_axis_args
    axis_ls <- plot.pretty::implement_pretty_axis_args(pretty_axis_args)

    #### Create boxplot, with appropriate limits
    graphics::boxplot(y ~ x,
                      xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim,
                      frame.plot = FALSE, axes = FALSE,
                      xlab = xlab, ylab = ylab,...)

    #### We could blank out the tick marks at the xlimits without labels by adding a line first,
    # ... without ticks, and then suppressing ticks
    xaxis <- axis_ls[[1]]$axis
    xaxis$lwd.ticks <- 0
    do.call(graphics::axis, xaxis)
    # Update axis_ls to only contain tick marks at factor positions:
    sel <- 2:(ng+1)
    axis_ls[[1]]$axis$at <- axis_ls[[1]]$axis$at[sel]
    axis_ls[[1]]$axis$labels <- axis_ls[[1]]$axis$labels[sel]

    #### Implement pretty_axis_args and implement_mtext_args
    plot.pretty::pretty_axis(axis_ls = axis_ls, add = TRUE)
    plot.pretty::implement_mtext_args(mtext_args)

    #### Hide output list of pretty_axis
    invisible()

  }

#### End of code.
######################################
######################################
