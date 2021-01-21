#' @title Pretty histograms
#' @description This function makes prettier histograms. Histograms are created using \code{\link[graphics]{hist}} with more flexibility for control over the axes. Unlike most functions in \code{\link[prettyGraphics]{prettyGraphics}}, \code{\link[prettyGraphics]{pretty_hist}} does not use \code{\link[prettyGraphics]{pretty_axis}} to draw axes. Instead, by default, the 'breaks' vector computed by \code{\link[graphics]{hist}} is used for the x axis, but every \code{xn}th value can be selected for inclusion as an axis tick mark. This ensures that axis breaks and tick marks are always aligned. Alternatively, x axis elements can be controlled using \code{xaxis}. The default y axis is defined with \code{\link[base]{pretty}} based on a named list of parameters provided by the user (e.g. including the approximate number of 'pretty' breaks). Alternatively, the y axis can be implemented via \code{yaxis} as for the x axis. Axis titles can be added using \code{xlab}, \code{ylab} or \code{main} or via \code{\link[graphics]{mtext}} via \code{mtext_args} which offers more control.
#'
#' @param x A numeric vector for which to create a histogram.
#' @param freq A logical input which defines whether or not to create a histogram of counts or probability densities.
#' @param xn A number which defines the distance between sequential breaks in \code{x} retained as x axis labels (i.e., every xnth break is retained as a label; see Description).
#' @param ypretty A named list of parameters, passed to \code{\link[base]{pretty}}, to create the y axis (see Description).
#' @param xaxis A named list of elements passed to \code{\link[graphics]{axis}} to adjust/create the x axis.
#' @param yaxis A named list of elements passed to \code{\link[graphics]{axis}} to adjust/create the y axis.
#' @param control_axis,control_sci_notation,control_digits Additional axis control arguments (see \code{\link[prettyGraphics]{pretty_axis}}).
#' @param xlim A vector of two numbers which define the lower and upper x limits of the histogram.
#' @param ylim A vector of two numbers which define the lower and upper y limits of the histogram.
#' @param xlab The x axis label. This can be added via \code{mtext_args} for more control.
#' @param ylab The y axis label. This can be added via \code{mtext_args} for more control.
#' @param main The plot title. This can be added via \code{mtext_args} for more control.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to add labels. A nested list is used to control each axis separately.
#' @param ... other parameters passed to \code{\link[graphics]{hist}}. Arguments that affect the axes should be implemented via \code{xaxis} or \code{yaxis}; any such arguments passed via \code{...} are silently ignored.
#'
#' @return The function returns a pretty histogram.
#'
#' @examples
#' #### Example (1) The default options
#' set.seed(1)
#' x <- rnorm(1000, 0, 1)
#' pretty_hist(x)
#'
#' #### Example (2) A density plot
#' pretty_hist(x, freq = FALSE)
#'
#' #### Example (3) Axes can be adjusted via xlim, ylim, breaks,
#' # ... xn, ypretty, or xaxis() and yaxis()
#' pp <- par(mfrow = c(1, 6))
#' pretty_hist(x, xlim = c(-10, 10), ylim = c(0, 1000))
#' pretty_hist(x, breaks = seq(-5, 5, by = 1), ylim = c(0, 1000))
#' pretty_hist(x, xn = 2)
#' pretty_hist(x, ypretty = list(n = 10))
#' pretty_hist(x, xaxis = list(at = -5:5))
#' pretty_hist(x, yaxis = list(at = seq(0, 300, by = 100)))
#' par(pp)
#'
#' #### Example (4) Axis labels can be adjusted via mtext() and mtext_args()
#' pp <- par(mfrow = c(1, 2))
#' pretty_hist(x, xlab = "xvar", ylab = "F", mtext_args = list())
#' pretty_hist(x,
#'             xlab = "", ylab = "",
#'             mtext_args = list(
#'                list(side = 1, "x var", line = 2),
#'                list(side = 2, "F", line = 2)))
#' par(pp)
#'
#' #### Example (5) Further  examples
#' pp <- par(mfrow = c(4, 2))
#' # e.g.
#' x <- c(1.466667, 1.500000)
#' pretty_hist(x)
#' hist(x)
#' # e.g.
#' x <- runif(100, 0, 0.1987)
#' pretty_hist(x)
#' hist(x)
#' # e.g.
#' x <- c(runif(100, 0, 0.1987), runif(100, 30, 500))
#' pretty_hist(x)
#' hist(x)
#' # e.g.
#' x <- c(1e10, 1e20, 1e30)
#' pretty_hist(x)
#' hist(x)
#' par(pp)
#'
#' #### Example (6) Some examples with dates and times
#' ## Define some time series data
#' x <- seq.Date(as.Date("2016-01-01"), as.Date("2017-01-01"), 1)
#' x <- sample(x, size = 1000, replace = TRUE)
#' ## Set plotting region
#' pp <- par(mfrow = c(2, 2))
#' ## hist() and pretty_hist() comparison
#' # Note that for times, hist() displays density by default,
#' # ... but freq = FALSE is needed for pretty_hist
#' hist(x, breaks = "months")
#' pretty_hist(x, breaks = "months", freq = FALSE)
#' ## Usually for time series, you need to be explicit about breaks
#' # ... for pretty plots:
#' hist(x,
#'      breaks = seq(min(x), max(x), by = "months"),
#'      format = "%d-%m", las = 2)
#' pretty_hist(x,
#'             breaks = seq(min(x), max(x), by = "months"),
#'             xaxis = list(format = "%d-%m", las = 2),
#'             freq = FALSE
#' )
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

###########################################
###########################################
#### pretty_hist()

pretty_hist <-
  function(x,
           freq = TRUE,
           xn = 1,
           ypretty = list(n = 5),
           xaxis = list(),
           yaxis = list(),
           xlim = NULL,
           ylim = NULL,
           xlab = deparse(substitute(x)),
           ylab = ifelse(freq, "Frequency", "Density"),
           main = "",
           control_axis = list(las = TRUE), control_sci_notation = list(), control_digits = NULL,
           mtext_args = list(),
           ...
  ){

    #### Checks
    check...("plot",...)
    mapply(list("ypretty", "xaxis", "yaxis"),
           list(ypretty, xaxis, yaxis),
           FUN = function(arg, input){
      check_input_class(arg = arg, input = input, to_class = "list", type = "stop")
      check_named_list(arg = arg, l = input, ignore_empty = TRUE)
    })

    #### Implement hist()
    if(!is.null(xaxis$at) & is.null(xlim)) xlim <- range(xaxis$at)
    if(!is.null(yaxis$at) & is.null(ylim)) ylim <- range(yaxis$at)
    param <- list(x = x, freq = freq, xlim = xlim, ylim = ylim, plot = FALSE)
    param <- list_merge(param, list(...))
    if(is_number(x)) param$warn.unused <- FALSE
    h <- do.call(graphics::hist, param)

    #### Update breaks
    convert_breaks <- function(x, breaks){
      if(is_number(x)){
        return(breaks)
      } else if(inherits(x, "Date")){
        return(as.Date(breaks, origin = "1970-01-01"))
      } else if(inherits(x, "POSIXct")){
        return(as.POSIXct(breaks, origin = "1970-01-01"))
      } else{
        stop("class(x) is unsupported: only classes numeric, integer, Date and POSIXct are supported.")
      }
    }
    h$breaks <- convert_breaks(x = x, h$breaks)

    #### Define select
    if(freq){
      select <- "counts"
    } else{
      select <- "density"
    }

    #### Define graph axis and limits
    if(is.null(xaxis$at)) {
      xaxis$at <- h$breaks
      if(!is.null(xlim)) xaxis$at <- seq_extend(xaxis$at, lim = xlim)
      xaxis$at <- xaxis$at[seq(1, length(xaxis$at), by = xn)]
      xaxis$labels <- pretty_labels(xaxis$at,
                                    xaxis$at,
                                    n = control_digits,
                                    sci_notation_args = control_sci_notation)
    }
    if(is.null(xlim)) xlim <- range(h$breaks)
    if(is.null(ylim)) ylim <- c(0, NA)
    yls <- pretty_seq(h[[select]], lim = ylim, pretty_args = ypretty)
    ylim <- yls$lim
    if(is.null(yaxis$at)) {
      yaxis$at <- yls$at
      yaxis$labels <- pretty_labels(yaxis$at,
                                    yaxis$at,
                                    n = control_digits,
                                    sci_notation_args = control_sci_notation)
    }
    if(is.null(xaxis$pos)) xaxis$pos <- ylim[1]
    if(is.null(yaxis$pos)) yaxis$pos <- xlim[1]
    xaxis <- list_merge(xaxis, control_axis)
    yaxis <- list_merge(yaxis, control_axis)

    #### Plot histogram
    graphics::hist(x,
                   freq = freq,
                   axes = FALSE,
                   xlim = xlim, ylim = ylim,
                   xlab = xlab, ylab = ylab, main = main,...
    )

    #### Add axes
    if(is.null(xaxis$side)) xaxis$side <- 1
    if(is.null(yaxis$side)) yaxis$side <- 2
    add_axis <- choose_foo_axis(xaxis$at)
    do.call(add_axis, xaxis)
    do.call(graphics::axis, yaxis)

    #### Add axis labels
    implement_mtext_args(mtext_args)

  } # close function


#### End of code.
###########################################
###########################################
