#' @title Prettier default plots
#' @description \code{\link[plot.pretty]{pretty_plot}} is a general function which creates plots with pretty axis for multiple plotting functions. The default option functions like \code{\link[graphics]{plot}} but implements pretty axes using \code{\link[plot.pretty]{pretty_axis}}. Arguments can be passed to \code{\link[graphics]{plot}}, but additional arguments (e.g., \code{points_args}, \code{lines_args} and \code{mtext_args}, which can be used to add points, lines and control axis labels respectively) provide additional flexibility if required. Some other plotting functions can be implemented by adjusting the plotting function, \code{f}, and by specifying whether that plot depends on \code{x}, \code{y} or \code{x} and \code{y}. The function can return the list from\code{\link[plot.pretty]{pretty_axis}} which can be useful for later adjustments to plots (e.g. setting the clipping region with \code{\link[graphics]{clip}}).
#'
#' @param x The x coordinates or an object from which coordinates can be extracted (see Details). Coordinates are used to create the axes and may be plotted (see \code{f} and \code{plot_coordinates}).
#' @param y (optional) The y coordinates.
#' @param f A function used to create a plot. The default is \code{\link[graphics]{plot}} but some other functions are supported. \code{f} should support the following arguments: \code{axes}, which are turned off automatically; \code{xlim}, \code{ylim}; \code{xlab}; \code{ylab}; and \code{main}. This feature is experimental.
#' @param plot_xy A character vector specifying whether the plotting function, \code{f}, requires \code{"x"}, \code{"y"} or both (\code{"xy"}).
#' @param plot_coordinates A logical input which defines whether or not the plotting function acts on coordinates extracted from \code{x} or the original object (see examples using \code{\link[raster]{image}}).
#' @param pretty_axis_args A named list or arguments passed to \code{\link[plot.pretty]{pretty_axis}} to define pretty axes.
#' @param points_args A named list of arguments passed to \code{\link[graphics]{points}} to control points. These can also be supplied via \code{...} but \code{point_args} can provide additional flexibility where required.
#' @param lines_args A named list of arguments passed to \code{\link[graphics]{lines}} to control lines. These can also be supplied via \code{...} but \code{lines_args} can provide additional flexibility where required.
#' @param xlab A character input which defines the label for the x axis. By default, this is "" so that labels can be added via \code{mtext_args} which is more flexible (see below). However, the label can be specified via \code{xlab} for speed.
#' @param ylab A character input which defines the label for the y axis. By default, this is "" so that labels can be added via \code{mtext_args} which is more flexible (see below). However, the label can be specified via \code{ylab} for speed.
#' @param main A character input which defines the label for the plot axis. By default, this is "" so that labels can be added via \code{mtext_args} which is more flexible (see below). However, the label can be specified via \code{main} for speed.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to control axes' labels. These can also be supplied via \code{xlab}, \code{ylab} and \code{main} but \code{mtext_args} can provide additional flexibility where required.
#' @param return_list A logical input which defines whether or not to return the list produced by \code{\link[plot.pretty]{pretty_axis}}.
#' @param ... Other parameters passed to \code{f}.
#'
#' @details \code{x} and \code{y} coordinates usually need to be provided. Some other object classes may be provided to \code{x}, from which x and y coordinates can be extracted to create axes. In this case, the user needs to indicate whether the plotting function, \code{f}, requires \code{x} and/or \code{y} and acts on extracted coordinates (\code{plot_coordinates = TRUE}) or the original object (\code{plot_coordinates = FALSE}). Objects of class density and RasterLayer are currently supported. If \code{plot_xy = "xy"} and only \code{x} is provided, \code{x} is treated as the response variable and plotted against an index (like \code{\link[graphics]{plot}}).
#'
#' @return The function returns a plot and, if requested, a list of arguments that are used to create pretty axes via \code{\link[plot.pretty]{pretty_axis}}.
#'
#' @examples
#' #### Example (1): An example with graphics::plot()
#' set.seed(1)
#' pretty_plot(stats::runif(10, 0, 1),
#'             stats::runif(10, 0, 1),
#'             points_args = list(pch = 21, col = scales::alpha("black", 0.8)))
#'
#' #### Example (2): pretty_plot() can also work with factors.
#' # ... As usual, the number of breaks can be controlled via pretty_axis_args:
#' ## Define data:
#' dx <- factor(LETTERS[1:10])
#' dy <- 1:10
#' ## Example plots:
#' pp <- par(mfrow = c(2, 2))
#' # Specify a break for every factor level via the pretty 'n' argument.
#' # ... (this can also be achieved via the units argument)
#' plot.pretty::pretty_plot(dx, dy,
#'                          pretty_axis_args = list(side = 1:2, pretty = list(n = 10))
#' )
#' # Specify a break for every other factor level via the pretty 'n' argument.
#' # ... (this can also be achieved via the units argument)
#' plot.pretty::pretty_plot(dx, dy,
#'                          pretty_axis_args = list(side = 1:2, pretty = list(n = 10/2))
#' )
#' # Comparisons to default plots:
#' graphics::plot(dx, dy)
#' graphics::plot.default(dx, dy)
#' par(pp)
#'
#' #### Example (3): If only x is provided, x is plotted against an index
#' pretty_plot(x = c(10, 20))
#'
#' #### Example (4): Coordinates usually need to be provided
#' # ... but pretty_plot() works with some other objects.
#' ## Density example
#' pretty_plot(density(stats::rnorm(100, 0, 1)), type = "l")
#' ## RasterLayer example
#' # Define example raster
#' r <- raster::raster(nrows=10, ncols=10)
#' r <- raster::setValues(r, 1:raster::ncell(r))
#' # Note the use of raster::image() rather than raster::plot() for plotting
#' # ... because raster::plot() doesn't act on xlim or ylim arguments defined by pretty_axis().
#' pretty_plot(x = r, y = NULL,
#'             f = raster::image, plot_xy = "x", plot_coordinates = FALSE)
#'
#' #### Example (5): An example with stats::qqnorm()
#' # Define x and y values for qqnorm plot
#' set.seed(1)
#' qq <- qqplot(stats::rnorm(100, 0, 1), stats::rnorm(100, 0, 1), plot.it = FALSE)
#' # Define plot, saving list of axis parameters in axis_ls
#' # Supply x and y (qq$x and qq$y) respectively to create pretty axis limits;
#' # ... but use qqnorm to create plot which only required y (dd$y) (see ?stats::qqnorm)
#' axis_ls <- pretty_plot(qq$x, qq$y, f = stats::qqnorm, plot_xy = "y", return_list = TRUE)
#' # Set clipping region with axis limits
#' usr <- par("usr")
#' clip(axis_ls[[1]]$lim[1], axis_ls[[1]]$lim[2], axis_ls[[2]]$lim[1], axis_ls[[2]]$lim[2])
#' # Add qqline within limits
#' qqline(qq$y)
#' do.call("clip", as.list(usr))
#'
#' @author Edward Lavender
#' @export
#'

#######################################
#######################################
#### pretty_plot

pretty_plot <-
  function(x,
           y = NULL,
           f = graphics::plot,
           plot_xy = "xy",
           plot_coordinates = TRUE,
           pretty_axis_args = list(side = 1:2, pretty = list(n = 5)),
           points_args = list(),
           lines_args = list(),
           xlab = "", ylab = "", main = "",
           mtext_args = list(),
           return_list = FALSE,...){

    #### Inital checks
    if(is.null(x)) stop("'x' is NULL.")
    if(!any(class(x) %in% c("numeric", "integer", "factor", "character", "Date", "POSIXct",
                            "density", "RasterLayer"))){
      stop("class(x) not currently supported.")
    }
    check_input_value(arg = "plot_xy", input = plot_xy, supp = c("x", "y", "xy"), default = "xy")
    check...("axes",...)

    #### Object inheritance for pretty_axis()
    xy <- pull_xy(x, y)
    if(plot_coordinates){
      x <- xy$x
      y <- xy$y
    }

    #### If y isn't supplied, plot x againist an index like graphics::plot()
    if(plot_xy == "xy" & is.null(xy$y)){
      message("'y' argument not supplied; 'x' is plotted against an index.")
      index <- 1:length(x)
      y <- xy$x
      x <- index
      # Redefine xy list for implement_pretty_axis_args, below.
      xy <- list(x = index, y = xy$x)
    }

    #### Implement pretty_axis_args
    axis_ls <- implement_pretty_axis_args(xy, pretty_axis_args)

    #### Variable type updates
    # Convert factors/characters to numbers for plotting
    if(is.factor(x) | is.character(x)){
      # warning("'x' converted to a number for plotting in pretty_plot().")
      x <- as.integer(factor(x))
    }
    if(is.factor(y) | is.character(y)){
      # warning("'y' converted to a number for plotting in pretty_plot().")
      y <- as.integer(factor(y))
    }

    #### Plot the graph
    if(plot_xy == "xy"){
      f(x, y,
        axes = FALSE,
        xlab = xlab, ylab = ylab, main = main,
        xlim = axis_ls[[1]]$lim,
        ylim = axis_ls[[2]]$lim,...
      )
    } else if(plot_xy == "x"){
      f(x,
        axes = FALSE,
        xlab = xlab, ylab = ylab, main = main,
        xlim = axis_ls[[1]]$lim,
        ylim = axis_ls[[2]]$lim,...
      )
    } else if(plot_xy == "y"){
      f(y,
        axes = FALSE,
        xlab = xlab, ylab = ylab, main = main,
        xlim = axis_ls[[1]]$lim,
        ylim = axis_ls[[2]]$lim,...
      )
    }

    #### Add points
    if(length(points_args) > 0){
      points_args$x <- x
      points_args$y <- y
      do.call(graphics::points, points_args)
    }

    #### Add lines
    if(length(lines_args) > 0){
      lines_args$x <- x
      lines_args$y <- y
      do.call(graphics::lines, lines_args)
    }

    #### Add pretty axis
    pretty_axis(axis_ls = axis_ls, add = TRUE, return_list = FALSE)

    #### Add axes labelling
    implement_mtext_args(mtext_args)

    #### Return list
    if(return_list) return(axis_ls)

  } # close pretty_plot()



#### End of code.
#######################################
#######################################
