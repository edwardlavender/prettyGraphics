#' @title Define a list containing fitted values and confidence intervals for the linear predictor or response from a model
#' @description This function creates a list of fitted values and lower/upper confidence intervals (CIs) that can be easily plotted from a list of fitted values and standard errors (SEs). For predictions on the scale of the response, the user is advised the supply the function with predictions on the scale of the link function (e.g. from \code{predict(..., type = "link")}) and an inverse link function. If an inverse link is supplied, the function first computes lower/upper CIs and then applies the inverse link function (this order of operations is generally preferable to the computation of SEs on the scale of the response (e.g. from \code{predict(..., type = "response")}) and then using these to compute CIs). Predictions can be adjusted with user-defined functions and the function can also return plot suggestions (e.g. y limits of a plot) based on predictions.
#'
#' @param pred A list of fitted values and corresponding SEs (usually from \code{\link[stats]{predict}}).
#' @param inv_link A function which defines the inverse link function. The default is \code{I} which leaves predictions unchanged. If supplied, the function applies \code{inv_link} after computing confidence intervals.
#' @param fadj A function by which to modify predictions (after the inverse link function has been applied, if applicable). For example, in models of animal depth time-series, models are often implemented by considering depth as a positive number, but plotted with depth as a negative number, for visualisation purposes.
#' @param centre A logical input defining whether or not to centre predictions. If \code{TRUE}, the mean fitted value is minused from predictions.
#' @param plot_suggestions A logical input defining whether or not to make plot suggestions (e.g. y limits) when can help make prettier plots.
#' @param pretty_param A list of parameters, passed to \code{\link[base]{pretty}}, if plot suggestions are requested. (\code{\link[prettyGraphics]{pretty_axis}} is more flexible.)
#'
#' @return A list with (usually) three elements: (1) 'fit', fitted values; (2) 'lowerCI', lower CIs (0.025 percentile); and (3) 'upperCI', upper CIs (97.5 percentile). If \code{plot_suggetions = TRUE}, 'yat' is a vector of suggested positions for y axis positions based on model predictions and 'ylim' is a vector of suggested y axis limits.
#'
#' @examples
#'
#' #### Example (1): A simple linear model
#' # Define some data for a model
#' set.seed(1)
#' x <- runif(1000, 0, 100)
#' y <- rnorm(1000, 0.5*x - 50, 100)
#' # Define model
#' m1 <- lm(y ~ x)
#' # Define predictions
#' p <- predict(m1, se.fit = TRUE)
#' # list CIs
#' CIs <- list_CIs(pred = p)
#' str(CIs)
#'
#'
#' @seealso \code{\link[base]{pretty}}, \code{\link[prettyGraphics]{pretty_axis}}, \code{\link[stats]{predict}}
#' @author Edward Lavender
#' @export


############################################
############################################
#### list_CIs

list_CIs <-
  function(
    # Supply output of predict(...type = "link", se.fit = TRUE)
    pred,
    inv_link = I,
    # An additional function to modify all predictions (e.g. * -1 for depth plots)
    fadj = I,
    # Define whether or not to centre the predictions; this may be useful
    # ... if inverse.link = TRUE,
    # ... if so, the mean value of the smooth is minused from the
    # ... predictions:
    centre = FALSE,
    # Define whether or not you want plot suggestions
    plot_suggestions = TRUE,
    # Plot suggestions are created using pretty function;
    # ...specify parameters in a list:
    pretty_param = list(n = 5)
  ){

    # Calculate fitted values on scale of response (if applicable) as well as
    # ... lower and upper CIs:
    fit <- inv_link(pred$fit)
    lowerCI <- inv_link(pred$fit - 1.96 * pred$se.fit)
    upperCI <- inv_link(pred$fit + 1.96 * pred$se.fit)
    # Group into a list:
    ls <- list(fit = fit,
               lowerCI = lowerCI,
               upperCI = upperCI
    )


    #### Process predictions
    ls <- lapply(ls, function(vals){ return(fadj(vals))})
    if(centre){
      mean_fit <- mean(fit)
      ls <- lapply(ls, function(vals){ return(vals - mean_fit)})
    }

    #### Make plot suggestions if required
    if(plot_suggestions){
      # Define minimum and maximum y values
      miny <- min(ls$lowerCI, na.rm = TRUE)
      maxy <- max(ls$upperCI, na.rm = TRUE)
      # Define yat using the pretty function and the parameter list supplied
      # ... for this function:
      pretty_param <- rlist::list.merge(list(x = c(miny, maxy)), pretty_param)
      yat <- do.call(pretty, pretty_param)
      # Define the difference between y axis values:
      yinterval <- abs(yat[2] - yat[1])
      # Adjust min and max values to ensure they are in the range of the data:
      if(min(yat) > miny) { yat <- sort(c(min(yat) - yinterval, yat)) }
      if(max(yat) < maxy) { yat <- sort(c(yat, max(yat) + yinterval)) }
      # Define ylim:
      ylim <- range(yat)
      # Define plot suggestions in a list:
      ls_plot <- list(yat = yat, ylim = ylim)
      # Join these suggestions to the original list:
      ls <- append(ls, ls_plot)
    } # close if(plot_suggestions){

    #### Return list:
    return(ls)
  } # close function


#### End of code.
#############################################
#############################################
