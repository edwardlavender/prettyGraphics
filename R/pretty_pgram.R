#' @title Pretty periodogram plots
#' @description This function produces a plot of a power spectrum. Power spectra can be estimated via \code{\link[stats]{spectrum}} or internally within this function. \code{\link[stats]{spectrum}} can also produce a plot of the power spectrum, but this function is designed to implement automatically some adjustments to this plot to facilitate interpretation. Specifically, under the default settings, this function (a) automatically scales the spectral density (by doubling) so that the area under the periodogram values equals the variance; (b) re-expresses frequencies in terms of the number of cycles per unit time (rather than the number of cycles per time interval), by dividing \code{\link[stats]{spectrum}}'s frequencies by the sampling interval (\code{sampling_interval}); (c) can plot frequency (\code{"f"}) or period (\code{"T"}) on the x-axis; (d) provided the flexibility to re-adjust the x axis units (via \code{x_units}). The plot is produced by \code{\link[prettyGraphics]{pretty_plot}} which can be customised via a named list of arguments that is passed to this function (\code{plot_args}).
#' @param x An object of class "spec" (see \code{\link[stats]{spectrum}}) or a univariate time series to be passed to \code{\link[stats]{spectrum}}.
#' @param sampling_interval A number that defines the sampling interval between sequential observations. If provided, this is used to re-express the frequencies that are returned by \code{\link[stats]{spectrum}} (where they are defined as the number of time cycles per time interval) into the number of cycles per unit time.
#' @param scale A logical input that defines whether or not to scale the spectral densities returned by \code{\link[stats]{spectrum}} (by doubling) so that the area under the periodogram values equals the variance.
#' @param log A logical input that defines whether or not to log (scaled) spectral densities. This is \code{FALSE} by default.
#' @param x_units A function that is used to process the x-axis units, after having converted frequencies into the number of cycles per unit time. For example, if the x-axis is given as frequency per minute, \code{function(x) x*60} will convert the x-axis into frequency per hour.
#' @param x_type A character input that specifies whether to plot frequencies (\code{"f"}) or periods (\code{"T"} = 1/f) on the x-axis.
#' @param plot_args A named list of arguments that is passed to \code{\link[prettyGraphics]{pretty_plot}} to customise the plot that is produced.
#' @param verbose A logical input that defines whether or not to print messages to the console defining the actions implemented.
#' @param ... Additional arguments to estimate the spectral density that are passed to \code{\link[stats]{spectrum}}, if \code{x} is a univariate time series.
#' @examples
#' # Define plotting window
#' pp <- par(mfrow = c(2, 3))
#' # Define a series of timestamps, with observations every 0.5 mins
#' delta <- 0.5
#' t <- seq(0, 30, by = delta)
#' # Simulate a response, with a period of 5 (i.e., 5 mins between every cycle)
#' y <- sin(((2*pi)/5)*t)
#' plot(t, y, type = "l", xlab = "Time (mins)")
#' # Visualise 'raw' periodogram
#' stats::spectrum(y, log = "no")
#' # Default pretty_pgram adjustments
#' # ... including converting frequency to per unit time (per minute)
#' # ... 0.2 cycles per minute = 1/0.2 = period of 5 mins:
#' pretty_pgram(y, sampling_interval = delta)
#' # Plot period instead of frequency (i.e., expect a period of 5 minutes)
#' pretty_pgram(y, sampling_interval = delta, x_type = "T")
#' # Convert period into seconds (one period per 5 mins = one period per 300 seconds)
#' pretty_pgram(y,
#'              sampling_interval = delta,
#'              x_type = "T",
#'              x_units = function(x) x/60
#'              )
#' par(pp)
#'
#' @return The function returns a plot of the spectral density (i.e., a spectral power plot).
#' @author Edward Lavender
#' @export
#'
pretty_pgram <- function(x,
                         sampling_interval = NULL,
                         scale = TRUE,
                         log = FALSE,
                         x_units = function(x) x,
                         x_type = "f",
                         plot_args = list(type = "l", col = "blue", xlab = x_type, ylab = "Spectral density"),
                         verbose = TRUE,...){

  #### Estimate spectral density, if required
  if(!inherits(x, "spec")) {
    if(verbose) cat("Estimating spectral density...\n")
    x <- stats::spectrum(x, plot = FALSE,...)
  }

  #### Extract coordinates
  x_spec <- x$freq
  y_spec <- x$spec

  #### Tapering and smoothing
  # ... implemented via spectrum(...)

  #### Scale spectral density
  if(scale) {
    if(verbose) cat("Scaling spectral density...\n")
    y_spec <- y_spec * 2
  }
  if(log) {
    if(verbose) cat("Logging spectral density...\n")
    y_spec <- log(y_spec)
  }

  #### Re-express frequencies
  ## Re-express frequencies per unit time
  x_type <- check_input_value(arg = "x_type",
                              input = x_type,
                              supp = c("f", "T"),
                              default = "f")
  if(!is.null(sampling_interval)){
    if(verbose) cat("Re-expressing frequencies per unit time...\n")
    x_spec <- x_spec/sampling_interval
  }
  ## Adjust units
  x_spec <- x_units(x_spec)
  ## Now convert to periods, if requested
  if(x_type == "T") {
    if(verbose) cat("Converting frequencies to periods...\n")
    x_spec <- 1/x_spec
  }

  #### Visualise plot
  plot_args$x <- x_spec
  plot_args$y <- y_spec
  if(is.null(plot_args$type)) plot_args$type <- "l"
  if(is.null(plot_args$xlab) & !is.null(plot_args$mtext_args)) plot_args$xlab <- x_type
  if(is.null(plot_args$ylab) & !is.null(plot_args$mtext_args)) plot_args$ylab <- "Spectral density"
  do.call(prettyGraphics::pretty_plot, plot_args)

}

