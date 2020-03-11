#' @title Pretty histograms
#' @description This function uses \code{link[plot.pretty]{pretty_axis}} to make prettier histograms.
#'
#' @param x A numeric vector for which to create a histogram.
#' @param select A character input (\code{"counts"} or \code{"density"}) specifying the type of histogram to draw.
#' @param pretty_axis_args A list of arguments passed to \code{\link[plot.pretty]{pretty_axis}} to define axes.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to add labels. A nested list is used to control each axis separately.
#' @param ... other parameters passed to \code{\link[graphics]{hist}} (e.g. if \code{select = "density"}, specify \code{freq = FALSE}).
#'
#' @return The function returns a pretty histogram.
#'
#' @examples
#'
#' #### Example (1) The default options
#' set.seed(1)
#' x <- rnorm(1000, 0, 1)
#' pretty_hist(x)
#'
#' #### Example (2) A density plot
#' pretty_hist(x, select = "density", freq = FALSE)
#'
#' #### Example (3) Axes can be adjusted via pretty_axis() with pretty_axis_args
#' pretty_hist(x, pretty_axis_args = list(pretty = list(n = 10), lim = list(range(x), NULL)))
#'
#' #### Example (4) Axis labels can be adjusted via mtext() and mtext_args()
#' pretty_hist(x, mtext_args = list(list(side = 1, "x var", line = 2), list(side = 2, "F", line = 2)))
#'
#' @author Edward Lavender
#' @export
#'

###########################################
###########################################
#### pretty_hist()

pretty_hist <-
  function(x,
           select = "counts",
           pretty_axis_args = list(side = 1:2, pretty = list(n = 10), axis = list(las = TRUE)),
           mtext_args = list(list(side = 1, text = "x", line = 2.5, cex = 1.3),
                             list(side = 2, text = "Frequency", line = 2.5, cex = 1.3)
                             ),
           ...
           ){


    #### Define histogram y values
    h <- graphics::hist(x, plot = FALSE, warn.unused = FALSE,...)
    y <- h[[select]]

    #### Define pretty axes parameters
    pretty_axis_args$x <- list(x = x, y = y)
    axis_ls <- implement_pretty_axis_args(pretty_axis_args)

    #### Plot histogram
    graphics::hist(x,
                axes = FALSE,
                xlim = axis_ls$`1`$lim, ylim = axis_ls$`2`$lim,
                xlab = "", ylab = "", main = "",...
                )

    #### Add axes
    pretty_axis(axis_ls = axis_ls, add = TRUE)

    #### Add axis labels
    implement_mtext_args(mtext_args)

  } # close function

#### End of code.
###########################################
###########################################
