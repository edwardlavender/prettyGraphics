#' @title Pretty probability density plots
#' @description The function is designed to produce pretty probability density plots. A rug is added for observed values.
#'
#' @param x A vector of quantiles.
#' @param f A density function.
#' @param param A named list of arguments required to evaluate the density function (see Examples).
#' @param pretty_axis_args A named list of arguments that are passed to \code{\link[plot.pretty]{pretty_axis}} to define axes.
#' @param mtext_args  A named list of arguments that are passed to \code{\link[graphics]{mtext}} to define axis labels.
#'
#' @return The function returns a pretty density plot.
#'
#' @examples
#' set.seed(1)
#' x <- runif(100, 1, 1000)
#' pretty_density(x = x, f = stats::dgamma, param = list(shape = 10, scale = 4))
#' pretty_density(x = x, f = stats::dgamma, param = list(shape = 11, scale = 3))
#' pretty_density(x = x, f = stats::dnorm, param = list(mean = 10, sd = 30))
#' pretty_density(x = x, f = stats::dnorm, param = list(mean = 10, sd = 30, log = TRUE))
#'
#' @author Edward Lavender
#' @export
#'

#######################################
#######################################
#### pretty_density()

pretty_density <-
  function(x,
           f = stats::dgamma,
           param = list(shape = 10, scale = 4),
           pretty_axis_args = list(side = 1:2, pretty = list(n = 5), axis = list(las = TRUE)),
           mtext_args = list(list(side = 1, text = "x", line = 2),
                             list(side = 2, text = "Density", line = 3)
                             )
           ){

    gammax <- seq(0, max(x), length.out = 100)
    param$x <- gammax
    gammay <- do.call(f, param)

    axis_ls <- implement_pretty_axis_args(list(gammax, gammay), pretty_axis_args)
    graphics::plot(gammax, gammay,
                   type = "n",
                   axes = FALSE,
                   xlab = "", ylab = "",
                   xlim = axis_ls[[1]]$lim, ylim = axis_ls[[2]]$lim)
    usr <- graphics::par("usr")
    graphics::clip(axis_ls[[1]]$lim[1], axis_ls[[1]]$lim[2], axis_ls[[2]]$lim[1], axis_ls[[2]]$lim[2])
    graphics::rug(x, pos = axis_ls[[2]]$lim[1], ticksize = 0.04, col = "royalblue", lwd = 2)
    graphics::lines(gammax, gammay, lwd = 1)
    pretty_axis(axis_ls = axis_ls, add = TRUE)
    implement_mtext_args(mtext_args)
    do.call("clip", as.list(usr))

  }


#### End of code.
#######################################
#######################################
