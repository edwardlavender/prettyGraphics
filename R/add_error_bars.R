#' @title Add error bars to a plot
#' @description This function is used to add error bars (and fitted values) to a plot. Error bars are drawn above and below fitted values.
#'
#' @param x The x values at which the error bars will be placed (e.g. factor levels).
#' @param y The y values around which error bars will be drawn (i.e., fitted values).
#' @param se A numeric value which defines the size of the error bar above and below fitted values. This can be adjusted with \code{scale}.
#' @param scale A numeric value by which to multiply the \code{se} (e.g. to convert standard errors into 95 percent confidence intervals).
#' @param length A numeric value which defines the length of the horizontal tips of the error bars. \code{length = 0} will suppress the horizontal tips of error bars.
#' @param add_fitted (optional) A named list of graphical parameters, passed to \code{\link[graphics]{points}} which, if provided, will add fitted values as points on top of error bars. An empty list (\code{add_fitted = list()}) will add fitted values to the plot using default graphical parameters.
#' @param ... Other arguments passed to \code{\link[graphics]{arrows}}, which is used to draw error bars, for customisation. Arguments \code{x0}, \code{x1}, \code{y0} and \code{y1} are obtained from \code{x}, \code{y}, \code{se} and \code{scale} and should not be provided. Likewise, \code{angle} is forced to be 90 degrees (i.e., error bars are forced to have horzontal tips (or no tips, if \code{length = 0})) and should not be provided.
#'
#' @examples
#' #### Simulate data
#' ## Simulation scenario
#' # Imagine a scenario where we have n observations for j factor levels
#' # Successive factor levels have expected values that differ by 10 units
#' # The observed response is normally distributed around expected values with some sd
#' ## Simulate data
#' set.seed(1)
#' n <- 30; j <- 3
#' fct <- sort(factor(rep(1:j, n)))
#' # Define means for each factor level
#' mean_vec <- seq(10, by = 10, length.out = j)
#' # Define expected values for each fct
#' mu <- stats::model.matrix(~fct - 1) %*% mean_vec
#' # Define simulated observations
#' sd <- 5
#' y <- stats::rnorm(length(mu), mu, sd)
#' ## Use a model to extract fitted values/coeffients
#' # Model data, using a means parameterisation for convenience
#' fit <- stats::lm(y ~ fct - 1)
#' # Extract fitted values and coefficients
#' coef <- summary(fit)$coefficients
#' fit <- coef[, 1]; fit
#' se <- coef[, 2]; se
#' # Define colour of points on subsequent plots
#' pch_col <- scales::alpha("royalblue", 0.4)
#'
#' #### Example (1): Add error bars using the default options:
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, y = fit, se = se)
#'
#' #### Example (2): Adjust error bars (e.g. to create 95 % CIs)
#' # ... either by pre-processing the se argument (e.g. *1.96) or via the scale argument,
#' # ... which implements the necessary multiplication internally:
#' pp <- par(mfrow = c(1, 2))
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, y = fit, se = se*1.96)
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, y = fit, se = se, scale = 1.96)
#' par(pp)
#'
#' #### Example (3): Customise error bars by supplying additional arguments via ...
#' # ... that are passed to graphics::arrows()
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, y = fit, se = se, length = 0.25, col = "red", lwd = 2)
#'
#' #### Example (4): Add the fitted points on by passing a list to the add_fitted argument:
#' pp <- par(mfrow = c(1, 2))
#' # Example with customised points:
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, y = fit, se = se, lwd = 2, add_fitted = list(pch = 21, bg = "black"))
#' # Simply specify an empty list to use the default options:
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, y = fit, se = se, lwd = 2, add_fitted = list())
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

##########################################
##########################################
#### add_error_bars()

add_error_bars <-
  function(
    x,
    y,
    se,
    scale = 1,
    length = 0.05,
    add_fitted = NULL,...
    ){

    #### Initial checks:
    stopifnot(length(x) == length(y))
    check...(not_allowed = c("x0", "y0", "x1", "y1", "code", "angle"))
    if(inherits(x, "factor")){
      warning("x values converted to a number for plotting.")
      x <- as.numeric(x)
    }

    #### Add SEs using arrows():
    graphics::arrows(x0 = x,
                     y0 = y - se*scale,
                     x1 = x,
                     y1 = y + se*scale,
                     code = 3,
                     angle = 90,
                     length = length,...)

    #### Add fitted points on top of error bars, if requested:
    if(!is.null(add_fitted)){
      if(!is.list(add_fitted)) stop("'add_fitted' argument must be a list.")
      add_fitted$x <- x
      add_fitted$y <- y
      do.call(graphics::points, add_fitted)
    }

  }


#### End of code.
##########################################
##########################################