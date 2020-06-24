#' @title Pretty function
#' @description The function evaluates a user-supplied function at range of user-supplied \code{x} values (and with any other named parameters), and produces a pretty plot of the evaluated function using \code{\link[plot.pretty]{pretty_plot}}. The function was particularly motivated by the need to visualise probability density functions (e.g. \code{\link[stats]{GammaDist}}.
#'
#' @param x The x coordinates at which a function, \code{f}, should be evaluated.
#' @param f A function which depends on a named argument, \code{x}, and any other named arguments (see \code{param}.)
#' @param param A named list of other arguments required to evaluate the function.
#' @param add_rug A named list of parameters, passed to \code{\link[graphics]{rug}} to add observed values to the plot. To add a rug using default parameters, simply specify \code{add_rug = list()}. If \code{x} values are not supplied in this list, they are taken from \code{x}. If \code{pos} is not supplied, the rug is positioned exactly along the x axis.
#' @param return_list A logical input which defines whether or not to return the list of axis parameters produced by \code{\link[plot.pretty]{pretty_axis}}.
#' @param ... Other parameters that are passed to \code{\link[plot.pretty]{pretty_plot}}, such as \code{pretty-axis_args} to adjust axes.
#'
#' @return The function evaluates a function and returns a plot.
#'
#' @examples
#' #### Generate some example x values
#' set.seed(1)
#' x <- seq(0, 100, length.out = 100)
#'
#' #### Simple examples
#' pretty_curve(x = x, f = stats::dgamma, param = list(shape = 10, scale = 4))
#' pretty_curve(x = x, f = stats::dgamma, param = list(shape = 11, scale = 3))
#' pretty_curve(x = x, f = stats::dnorm, param = list(mean = 10, sd = 30))
#' pretty_curve(x = x, f = stats::dnorm, param = list(mean = 10, sd = 30, log = TRUE))
#'
#' #### Customise plot by passing arguments to pretty_plot()
#' pretty_curve(x, f = stats::dgamma, param = list(shape = 10, scale = 4),
#'              pretty_axis_args = list(axis = list(las = TRUE)),
#'              col = "red", type = "l")
#'
#' #### Add a rug with observed data
#' # Default is to take 'x' as observed data
#' pretty_curve(x, f = stats::dgamma, param = list(shape = 10, scale = 4),
#'              add_rug = list())
#' # Specify rug parameters
#' pretty_curve(x, f = stats::dgamma, param = list(shape = 10, scale = 4),
#'              add_rug = list(x = seq(0, 100, by = 10), col = "red", lwd = 2))
#'
#' @seealso \code{\link[graphics]{curve}} for a similar function in base R.
#' @author Edward Lavender
#' @export
#'

#######################################
#######################################
#### pretty_curve()

pretty_curve <-
  function(x,
           f,
           param = list(),
           add_rug = NULL,
           return_list = FALSE,
           ...){

    #### Checks
    check...("return_list",...)

    #### Evaluate the function
    # gammax <- seq(0, max(x), length.out = 100)
    param$x <- x
    y <- do.call(f, param)

    #### Plot graph
    axis_ls <- pretty_plot(x, y, return_list = TRUE,...)

    #### Add rug
    if(is.list(add_rug)){
      add_list <- check_named_list(arg = "add_rug", l = add_rug, ignore_empty = TRUE)
      if(is.null(add_rug$x)) add_rug$x <- x
      if(is.null(add_rug$pos)) add_rug$pos <- axis_ls[[1]]$lim[1]
      do.call(graphics::rug, add_rug)
    }

    #### Return list of axis parameters
    if(return_list) return(axis_ls)

  }


#### End of code.
#######################################
#######################################
