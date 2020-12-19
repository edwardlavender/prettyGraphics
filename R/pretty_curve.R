#' @title Pretty function plots
#' @description The function evaluates a user-supplied function at user-supplied \code{x} values (or an interpolated sequence of values across \code{x} and for defined values for other named parameters), and produces a pretty plot of the evaluated function using \code{\link[prettyGraphics]{pretty_plot}}. The function was particularly motivated by the need to visualise probability density functions (e.g. \code{\link[stats]{GammaDist}}).
#'
#' @param x The x coordinates at which a function, \code{f}, should be evaluated. These may be interpolated between a user-specified range defined by user-specified limits depending on inputs to \code{x_interp}, \code{from}, \code{to} and \code{n} (see below).
#' @param x_interp A logical input that defines whether or not \code{x} values should be interpolated between limits before function evaluation.
#' @param from,to,n If \code{x_interp = TRUE}, \code{from}, \code{to} and \code{n} specify the range and the number of values between these limits at which the function is evaluated. By default, \code{x_interp = TRUE} and the function is evaluated for \code{n = 101} values spanning the range of \code{x}
#' @param x_interp A logical input that defines whether or not a regular sequence of coordinates should be interpolated within the limits of \code{x} at which the function is evaluated. This is only implemented if \code{x} is not a sorted sequence.
#' @param f A function which depends on a named argument, \code{x}, and any other named arguments (see \code{param}).
#' @param param A named list of other arguments required to evaluate the function.
#' @param xlab,ylab The x and y axis labels. These can also be added via the \code{mtext_args} argument of \code{\link[prettyGraphics]{pretty_plot}} via \code{...}.
#' @param type A character that specifies the plot type (see \code{\link[graphics]{plot.default}}).
#' @param add_rug A named list of parameters, passed to \code{\link[graphics]{rug}} to add observed values to the plot. To add a rug using default parameters, simply specify \code{add_rug = list()}. If \code{x} values are not supplied in this list, they are taken from \code{x}. If \code{pos} is not supplied, the rug is positioned exactly along the x axis.
#' @param return_list A logical input which defines whether or not to return the list of axis parameters produced by \code{\link[prettyGraphics]{pretty_axis}}.
#' @param ... Other parameters that are passed to \code{\link[prettyGraphics]{pretty_plot}}, such as \code{pretty_axis_args} to adjust axes.
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
           x_interp = TRUE,
           from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE), n = 101,
           f,
           param = list(),
           xlab = "x",
           ylab = paste0("f(", paste0(c("x", names(param)), collapse = ", "), ")"),
           type = "l",
           add_rug = NULL,
           return_list = FALSE,
           ...){

    #### Checks
    check...("return_list",...)

    #### Evaluate the function
    if(x_interp) x <- seq(from, to, length.out = n)
    param$x <- x
    y <- do.call(f, param)
    param$x <- NULL

    #### Plot graph
    axis_ls <- pretty_plot(x, y, xlab = xlab, ylab = ylab, type = type, return_list = TRUE,...)

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
