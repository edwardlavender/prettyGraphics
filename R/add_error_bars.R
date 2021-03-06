#' @title Add error bars to a plot
#' @description This function is used to add error bars (and fitted values) to a plot. Fitted values are marked as points (if requested) around which error bars are drawn (horizontally or vertically as appropriate). Error bars can be specified via the values of the explanatory variable (\code{x}), the fitted values (\code{fit}) and (a) their standard errors (which are multiplied by a user-defined \code{scale} parameter to define the limits of each error bar) or (b) the lower and upper values of each error bar (\code{lwr} and \code{upr}) directly.
#'
#' @param x The values of an explanatory variable at which the error bars will be placed (e.g. factor levels).
#' @param fit The fitted values from a model, around which error bars will be drawn.
#' @param se,scale A number or vector of numbers that define(s) the standard error(s) (\code{se}) and a scaling parameter (\code{scale}) that, together, define the size of the error bars above and below fitted values. Standard errors are multiplied by the scaling parameter (e.g. to convert standard errors into 95 percent confidence intervals). This results in symmetric error bars. Alternatively, the lower and upper limits of the error bars can be defined directly via \code{lwr} and \code{upr}.
#' @param lwr,upr A vector of numbers, of the same length as \code{x}, that defines the lower (\code{lwr}) and upper (\code{upr}) limits of the error bars. By default, these are defined using the \code{se} and \code{scale} arguments, but they can be defined by the user instead (e.g., if a model returns 95 percent confidence intervals, rather than standard errors, or if error bars are asymmetrical).
#' @param horiz A logical value that defines whether or not error bars will be drawn vertically (\code{FALSE}) or horizontally (\code{TRUE}). For vertical error bars, \code{x} and \code{fit} are taken as the x positions at/around which error bars are drawn; this is reversed for horizontal error bars.
#' @param length A numeric value which defines the length of the horizontal tips of the error bars. \code{length = 0} will suppress the horizontal tips of error bars.
#' @param add_fit A named list of graphical parameters, passed to \code{\link[graphics]{points}} which, if provided, will add fitted values as points on top of error bars. An empty list (\code{add_fit = list()}) will add fitted values to the plot using default graphical parameters.
#' @param add_fitted (depreciated) See \code{add_fit} argument.
#' @param ... Other arguments passed to \code{\link[graphics]{arrows}}, which is used to draw error bars, for customisation. Arguments \code{x0}, \code{x1}, \code{y0} and \code{y1} are obtained from \code{x}, \code{fit}, and \code{se} and \code{scale}, or \code{lwr} and \code{upr}, and should not be provided. Likewise, \code{angle} is forced to be 90 degrees (i.e., error bars are forced to have horizontal tips (or no tips, if \code{length = 0})) and should not be provided.
#'
#' @seealso This function is designed for discrete explanatory variables. \code{\link[prettyGraphics]{add_error_envelope}} is used for continuous explanatory variables to add regression lines and associated error envelopes to plots.
#'
#' @examples
#' #### Example dataframe
#' # Define a hypothetical dataframe with predictor values and fitted values
#' sim <- data.frame(type = factor(LETTERS[1:3]),
#'                   fit = c(20, 30, 40))
#'
#' #### Example (1): Vertical error bars
#'
#' ## Vertical error bars based on SE
#' # Visualise vertical error bars based on (hypothetical) SE
#' pretty_plot(sim$type, sim$fit, type = "n", ylim = c(0, 50))
#' add_error_bars(sim$type, sim$fit, se = 5, lwd = 3)
#' # Adjust SE before implementing the function to show CIs
#' pretty_plot(sim$type, sim$fit, type = "n", ylim = c(0, 50))
#' add_error_bars(sim$type, sim$fit, se = 5*1.96)
#' # Or simply adjust the arument to 'scale'
#' pretty_plot(sim$type, sim$fit, type = "n", ylim = c(0, 50))
#' add_error_bars(sim$type, sim$fit, se = 5*1.96)
#'
#' ## Vertical error bars based on lwr and upper estimates
#' sim$lwr <- c(10, 5, 35)
#' sim$upr <- c(30, 40, 45)
#' pretty_plot(sim$type, sim$fit, type = "n", ylim = c(0, 50))
#' add_error_bars(sim$type, sim$fit, lwr = sim$lwr, upr = sim$upr)
#'
#' #### Example (2): Horizontal error bars
#'
#' ## Horizontal error bars (via horiz = TRUE) based on SEs
#' pretty_plot(sim$fit, sim$type, type = "n", xlim = c(0, 50))
#' add_error_bars(sim$type, sim$fit, se = 5, horiz = TRUE)
#'
#' ## Horizontal error bars (via horiz = TRUE) based on lwr and upr estimates
#' pretty_plot(sim$fit, sim$type, type = "n", xlim = c(0, 50))
#' add_error_bars(sim$type, sim$fit, lwr = sim$lwr, upr = sim$upr, horiz = TRUE)
#'
#' #### Example (3) Customise error bars by supplying additional arguments via ...
#' # ... that are passed to graphics::arrows()
#' pretty_plot(sim$type, sim$fit)
#' add_error_bars(x = sim$type, fit = sim$fit, se = 5,
#'                length = 0.25, col = "red", lwd = 2)
#'
#' #### Example (4): Add the fitted points on by passing a list to the add_fit argument:
#' pp <- par(mfrow = c(1, 2))
#' # Example with customised points:
#' pretty_plot(sim$type, sim$fit)
#' add_error_bars(x = sim$type, fit = sim$fit, se = 5, lwd = 2,
#'                add_fit = list(pch = 21, bg = "black"))
#' # Simply specify an empty list to use the default options:
#' pretty_plot(sim$type, sim$fit)
#' add_error_bars(x = sim$type, fit = sim$fit, se = 5, lwd = 2,
#'                add_fit = list())
#' par(pp)
#'
#' #### Example (5): More realistic example using model derived estimates
#'
#' ## Simulate data
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
#'
#' ## Use a model to extract fitted values/coefficients
#' # Model data, using a means parameterisation for convenience
#' fit <- stats::lm(y ~ fct - 1)
#' # Extract fitted values and coefficients
#' coef <- summary(fit)$coefficients
#' fit <- coef[, 1]; fit
#' se <- coef[, 2]; se
#'
#' ## Add error bars using the default options:
#' pch_col <- scales::alpha("royalblue", 0.4)
#' pretty_plot(fct, y, pch = 21, col = pch_col, bg = pch_col)
#' add_error_bars(x = 1:3, fit = fit, se = se)
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
    fit,
    se = stats::sd(fit), scale = 1,
    lwr = fit-scale*se, upr = fit+scale*se,
    length = 0.05,
    horiz = FALSE,
    add_fit = list(pch = 21, bg = "black"), add_fitted = NULL,...
    ){

    #### Initial checks:
    stopifnot(length(x) == length(fit))
    check...(not_allowed = c("x0", "y0", "x1", "y1", "code", "angle"))
    if(inherits(x, "factor")){
      message("x values converted to a number for plotting.")
      x <- as.numeric(x)
    }

    #### Add SEs using arrows():
    if(!horiz){
      x0 <- x
      y0 <- lwr
      x1 <- x
      y1 <- upr
    } else{
      x0 <- lwr
      y0 <- x
      x1 <- upr
      y1 <- x
      x <- fit
      fit <- x
    }
    graphics::arrows(x0 = x0,
                     y0 = y0,
                     x1 = x1,
                     y1 = y1,
                     code = 3,
                     angle = 90,
                     length = length,...)

    #### Add fitted points on top of error bars, if requested:
    if(!is.null(add_fitted)) {
      warning("The 'add_fitted' argument has been renamed 'add_fit' for consistency.")
      add_fit <- add_fitted
    }
    if(!is.null(add_fit)){
      if(!is.list(add_fit)) stop("'add_fit' argument must be a list.")
      add_fit$x <- x
      add_fit$y <- fit
      do.call(graphics::points, add_fit)
    }

  }


#### End of code.
##########################################
##########################################
