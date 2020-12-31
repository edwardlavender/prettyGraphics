#' @title Add a colour bar legend to a plot
#' @description This function adds a colour bar legend to a plot. The function accompanies \code{\link[prettyGraphics]{add_lines}} and can be used to add a legend which corresponds to the lines drawn. This can be added to an existing plot using \code{\link[TeachingDemos]{subplot}}.
#'
#' @param data_legend A dataframe with two columns named 'x' and 'col'. 'x' is a regular sequence of values. 'col' is the colour of each value. This is used to define the colour bar. \code{data_legend} can be extracted from \code{\link[prettyGraphics]{add_lines}}.
#' @param pretty_axis_args A list of arguments passed to \code{\link[prettyGraphics]{pretty_axis}} via the \code{axis_ls} argument to add an axis to the colour bar, defining the meaning of different colours.
#' @param mtext_args A list of arguments passed to \code{\link[graphics]{mtext}} to add a label to the legend.
#' @param data_raw (optional) A numeric vector that contains the raw data to which the the colour bar refers. If provided, \code{data_raw} is used to calculate the range of the raw data; this can be added to the legend via \code{mark_args} (see below).
#' @param mark_args (optional) A list of arguments passed to \code{\link[graphics]{lines}} (excluding \code{x} and \code{y}). If provided, these arguments are used to draw lines on the colour bar delineating the range of the raw data. This can be useful in circumstances in which the range of the colour bar differs from the raw data (the user has control over this; see Examples). A single list affects both the lower and upper limit delimiters identically; a nested list with two elements controls the lower and upper limit independently.
#'
#' @return The function return a plot of the colour bar. This can be added to an existing plot with \code{\link[TeachingDemos]{subplot}}.
#'
#' @examples
#' #### Define some data
#' set.seed(1)
#' x <- c(-20, 9, 10:987, 1200)
#' y1 <- runif(length(x), 0, 100)
#' y2 <- x*3-1000
#'
#' #### Plot graph and add colour line, saving output of add_lines to a list:
#' plot(x, y1)
#' colour_line_ls <- add_lines(x, y1, y2)
#'
#' #### Example (1): add_colour_bar uses outputs of add_lines and returns a plot
#' add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                pretty_axis_args = colour_line_ls$axis_legend
#' )

#' #### Example (2): add_colour_bar can be used in combination with TeachingDemos::subplot()
#' pp <- par(oma = c(2, 2, 2, 6))
#' plot(x, y1, type = "n"); add_lines(x, y1, y2)
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#'
#' #### Example (3): the mtext_args argument can be used to add a label
#' pp <- par(oma = c(2, 2, 2, 6))
#' plot(x, y1, type = "n"); add_lines(x, y1, y2)
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend,
#'                               mtext_args = list(side = 4,
#'                                                 text = "Legend",
#'                                                 cex = 1.5,
#'                                                 font = 2,
#'                                                 line = 3),
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#' #### Example (4): data_raw and mark_args can be used to delineate the range of data
#' pp <- par(oma = c(2, 2, 2, 6))
#' plot(x, y1, type = "n"); add_lines(x, y1, y2)
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend,
#'                               data_raw = y2,
#'                               mark_args = list(col = "dimgrey", lwd = 2)
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#' #### Example (5): the lower and upper data delimiters can be controlled independently
#' # ... using a nested list:
#' pp <- par(oma = c(2, 2, 2, 6))
#' plot(x, y1, type = "n"); add_lines(x, y1, y2)
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend,
#'                               data_raw = y2,
#'                               mark_args = list(list(col = "dimgrey"), list(col = "green"))
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#'
#' #### Example (6): the axis harnesses the power of pretty_axis() and can be adjusted
#' # For example, in Example 5, it is clear that the colour bar extends
#' # ... beyond the range of the data. This is the default behaviour for pretty_axis()
#' # .... but we can force pretty labels to lie within the range of the data by adjusting
#' # ... the initial call to add_lines() to incorporate fixed limits
#' # ... in the pretty_axis_args argument:
#' par(oma = c(2, 2, 2, 6))
#' plot(x, y1)
#' colour_line_ls <- add_lines(x,
#'                             y1,
#'                             y2,
#'                             pretty_axis_args = list(lim = list(range(y2)),
#'                                                     pretty = list(n = 5))
#'                                                     )
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#' #### Example (7): another option is to increase the number of pretty breaks
#' # ... so the pretty axis and limits of the data line up well:
#' par(oma = c(2, 2, 2, 6))
#' plot(x, y1)
#' colour_line_ls <- add_lines(x, y1, y2, pretty_axis_args = list(pretty = list(n = 20)))
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#' #### Example (8): other axis customisation options are best handled via pretty_axis_args, e.g.:
#' par(oma = c(2, 2, 2, 6))
#' plot(x, y1)
#' colour_line_ls <- add_lines(x,
#'                             y1,
#'                             y2,
#'                             pretty_axis_args = list(pretty = list(n = 20),
#'                                                     axis = list(las = 2)))
#' TeachingDemos::subplot(add_colour_bar(data_legend = colour_line_ls$data_legend,
#'                               pretty_axis_args = colour_line_ls$axis_legend
#' ),
#' x = 1275, y = 0, size = c(0.25, 2.75), vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

################################################
################################################
#### add_colour_bar()

add_colour_bar <-
  function(data_legend,
           pretty_axis_args = list(),
           mtext_args = list(),
           data_raw = NULL,
           mark_args = list()
           ){

    #### Define limits and number of points
    lim <- range(data_legend$x)
    lcs <- nrow(data_legend)

    #### Create a blank plot, setting limits appropriately
    graphics::plot(c(0,1), lim,
                   xlim = c(0, 1),
                   ylim = lim,
                   type = "n",
                   axes = F,
                   xlab = "", ylab = "",
                   main = "")

    #### Add a colour bar
    # For every colour in the colour palette (except the last one...
    for(i in 1:(lcs - 1)){
      # we will draw a rectangle and fill it with the appropriate colour
      graphics::rect(xleft = 0,
                     ybottom = data_legend$x[i],
                     xright = 1,
                     ytop = data_legend$x[i + 1],
                     col = data_legend$col[i],
                     border = NA)
    } # close for loop: for(i in 1:(lcs - 1)){

    #### If mark upper and lower data_legend limits
    if(length(mark_args) > 0){
      stopifnot(!is.null(data_raw))
      data_raw_lim <- range(data_raw, na.rm = TRUE)
      if(list_depth(mark_args) == 1){
        mark_args <- list(mark_args, mark_args)
      }
      mapply(data_raw_lim, mark_args, FUN = function(ilim, mark_args1){
        mark_args1 <- rlist::list.merge(mark_args1, list(x = c(0, 1), y = c(ilim, ilim)))
        do.call(graphics::lines, mark_args1)
      })
    }

    #### Add legend axis using axis
    # use pretty_axis here, even though we're only considering one axis
    # ... because pretty_axis also draws a line across the full range of the colour bar
    # ... if user-specified limits
    if(length(pretty_axis_args) > 0){
      pretty_axis(axis_ls = pretty_axis_args, add = TRUE)
    }

    #### Add the axis label
    if(length(mtext_args) > 0){
      do.call("mtext", mtext_args)
    }

  } # End of function.


#### End of function.
################################################
################################################
