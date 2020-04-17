#' @title Pretty histograms
#' @description This function makes prettier histograms. Histograms are created using \code{\link[graphics]{hist}} with more flexibility for control over the axes. By default, the 'breaks' vector computed by \code{\link[graphics]{hist}} is used for the x axis, but every \code{xn} value can be selected for inclusion as an axis tick mark. Alternatively, x axis elements can be controlled using \code{xaxis}. The default y axis is defined with \code{\link[base]{pretty}} based on a named list of parameters provided by the user (e.g. including the approximate number of 'pretty' breaks). Alternatively, the y axis can be implemented via \code{yaxis} as for the x axis. Axis titles can be added using \code{xlab}, \code{ylab} or \code{main} or via \code{\link[graphics]{mtext}} via \code{mtext_args} which offers more control.
#'
#' @param x A numeric vector for which to create a histogram.
#' @param freq A logical input which defines whether or not to create a histogram of counts or probability densities.
#' @param xn A number which defines the distance between sequential breaks in \code{x} retained as x axis labels (i.e., every xnth break is retained as a label; see Description).
#' @param ypretty A named list of parameters, passed to \code{\link[base]{pretty}} to create the y axis (see Description).
#' @param xaxis A named list of elements passed to \code{\link[graphics]{axis}} to adjust/create the x axis.
#' @param yaxis A named list of elements passed to \code{\link[graphics]{axis}} to adjust/create the y axis.
#' @param xlim A vector of two numbers which define the lower and upper x limits of the histogram.
#' @param ylim A vector of two numbers which define the lower and upper y limits of the histogram.
#' @param xlab The x axis label. This can be added via \code{mtext_args} for more control.
#' @param ylab The y axis label. This can be added via \code{mtext_args} for more control.
#' @param main The plot title. This can be added via \code{mtext_args} for more control.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to add labels. A nested list is used to control each axis separately.
#' @param ... other parameters passed to \code{\link[graphics]{hist}}.
#'
#' @return The function returns a pretty histogram.
#'
#' @details By default, the x axis is a sequence of values
#'
#' @examples
#'
#' #### Example (1) The default options
#' set.seed(1)
#' x <- rnorm(1000, 0, 1)
#' pretty_hist(x)
#'
#' #### Example (2) A density plot
#' pretty_hist(x, freq = FALSE)
#'
#' #### Example (3) Axes can be adjusted via xn, ypretty, or xaxis() and yaxis()
#' pp <- par(mfrow = c(1, 3))
#' pretty_hist(x, xn = 2)
#' pretty_hist(x, ypretty = list(n = 10))
#' pretty_hist(x, xaxis = list(at = -5:5), xlim = c(-5, 5))
#' par(pp)
#'
#' #### Example (4) Axis labels can be adjusted via mtext() and mtext_args()
#' pp <- par(mfrow = c(1, 2))
#' pretty_hist(x, xlab = "xvar", ylab = "F", mtext_args = list())
#' pretty_hist(x,
#'             mtext_args = list(
#'                list(side = 1, "x var", line = 2),
#'                list(side = 2, "F", line = 2)))
#' par(pp)
#'
#' #### Example (5) Further  examples
#' pp <- par(mfrow = c(3, 2))
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
           freq = FALSE,
           xn = 1,
           ypretty = list(n = 5),
           xaxis = list(),
           yaxis = list(las = TRUE),
           xlim = NULL,
           ylim = NULL,
           xlab = "",
           ylab = "",
           main = "",
           mtext_args = list(list(side = 1, text = "x", line = 2.5),
                             list(side = 2, text = "Frequency", line = 2.5)
           ),
           ...
           ){

    #### Define pretty axes parameters
    h <- graphics::hist(x,
                        freq = freq,
                        plot = FALSE,
                        warn.unused = FALSE,...)

    #### Define select
    if(freq){
      select <- "counts"
    } else{
      select <- "density"
    }

    #### Define graph axis and limits
    if(is.null(xaxis$at)) {
      xaxis$at <- h$breaks
      xaxis$at <- xaxis$at[seq(1, length(xaxis$at), by = xn)]
    }
    if(is.null(xlim)) xlim <- range(h$breaks)
    ypretty$x <- c(0, max(h[[select]]))
    if(length(unique(ypretty$x)) == 1) ypretty$x[2] <- ypretty$x[2] + 1
    if(is.null(yaxis$at)) yaxis$at <- do.call("pretty", ypretty)
    if(is.null(ylim)) ylim <- range(yaxis$at)
    if(is.null(xaxis$pos)) xaxis$pos <- ylim[1]
    if(is.null(yaxis$pos)) yaxis$pos <- xlim[1]

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
    do.call(graphics::axis, xaxis)
    do.call(graphics::axis, yaxis)

    #### Add axis labels
    implement_mtext_args(mtext_args)

  } # close function

#### End of code.
###########################################
###########################################
