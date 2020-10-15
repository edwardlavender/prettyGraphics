#' @title Visualise the within and between day variation in a timeseries
#' @description This function produces a 2-dimensional time plot of a response variable. The surface of the plot shows the response variable in relation to time of day on one axis (usually the x axis) and time of year (specifically, the date) on another axis (usually the y axis). The function was motivated by the need to visualise how the depth of aquatic animals changes over the course of the day and how these patterns change over the course of the year (e.g., as in Teo et al., 2013).
#' @param x A vector of timestamps in POSIXct (\code{\link[base]{DateTimeClasses}}) format.
#' @param y A numeric vector of values of the response variable.
#' @param res A number which defines the resolution (in minutes) between sequential observations.
#' @param t1_units A function which defines the units of the axis of within-day variation. Since \code{res} is in minutes, \code{function(x) x} results in units of minutes, while the default \code{function(x) x/60} results in units of hours.
#' @param t2_units A function which defines the format of the axis of between-day variation. The function takes in objects of class \code{\link[base]{Date}} and returns axis labels. The default option is to retain dates in YYYY-MM-DD format, but other formats (e.g., Julian day) can be expressed via this option.
#' @param retain_orientation A logical input which defines whether to plot time of day against time of year (\code{TRUE}) or vice-versa (\code{FALSE}).
#' @param xtick_every_n A numeric input which defines the spacing between sequential tick marks for the x axis (see \code{\link[prettyGraphics]{pretty_mat}}).
#' @param ytick_every_n A numeric input which defines the spacing between sequential tick marks for the x axis (see \code{\link[prettyGraphics]{pretty_mat}}).
#' @param xlab The x axis label.
#' @param ylab The y axis label.
#' @param return_mat A logical input which defines whether or not to return the matrix plotted.
#' @param verbose A logical input which defines whether or not to print messages to the console. This can be useful in monitoring function progress, especially for large datasets.
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{pretty_mat}} for plot customisation.
#'
#' @return The function returns a plot and, if requested, a matrix of time of day (in user-specified units but hours by default) x time of year (in user-specified units but days by default).
#'
#' @seealso The plot is produced by \code{\link[prettyGraphics]{pretty_mat}}.
#'
#' @references
#' Teo, S.L., Sandstrom, P.T., Chapman, E.D., Null, R.E., Brown, K., Klimley, A.P., Block, B.A., 2013. Archival and acoustic tags reveal the post-spawning migrations, diving behav- ior, and thermal habitat of hatchery-origin Sacramento River steelhead kelts (\emph{Oncorhynchus mykiss}). Environ. Biol. Fish 96, 175–187.
#'
#' @examples
#' #### Define an example dataframe
#' dfa <- dat_flapper[dat_flapper$id == "A", ]
#'
#' #### Example (1): Visualise plot for different response variables
#' # ... sampled at 2 minutes resolution
#' pretty_ts_mat(dfa$timestamp, dfa$depth, res = 2)
#' pretty_ts_mat(dfa$timestamp, dfa$temp, res = 2)
#'
#' #### Example (2): Adjust the units on the time of day axis via t1_units
#' pretty_ts_mat(dfa$timestamp, dfa$depth,
#'                   res = 2,
#'                   t1_units = function(x) x,
#'                   ylab = "Time (mins)")
#'
#' #### Example (3): Adjust the format of the time of year axis via t2_units
#' # Julian day
#' pretty_ts_mat(dfa$timestamp, dfa$depth,
#'                   res = 2,
#'                   t2_units = function(x) lubridate::yday(x))
#' # day-month
#' pretty_ts_mat(dfa$timestamp, dfa$depth,
#'               res = 2,
#'               t2_units = function(x) format(x, "%d-%m"))
#'
#' #### Example (4): Adjust the number of tick marks via xtick_every_n and ytick_every_n
#' pretty_ts_mat(dfa$timestamp, dfa$depth,
#'                   res = 2,
#'                   t1_units = function(x) x,
#'                   ytick_every = 120,
#'                   ylab = "Time (mins)")
#' # Other customisation is implemented via ... (see ?prettyGraphics::pretty_mat)
#'
#' @author Edward Lavender
#' @export
#'

pretty_ts_mat <-
  function(x, y,
           res,
           t1_units = function(x) x/60,
           t2_units = function(x) as.character(x),
           retain_orientation = TRUE,
           xtick_every_n = 5,
           ytick_every_n = 60/res,
           xlab = "Time (days)",
           ylab = "Time (hours)",
           return_mat = FALSE,
           verbose = TRUE,...){

    #### Checks
    if(verbose) cat("Step 1: Set up...\n")
    dat <- data.frame(timestamp = x, response = y)

    #### Define time units
    if(verbose) cat("Step 2: Defining time categories...\n")
    # The number of minutes since midnight
    dat$min  <- lubridate::hour(dat$timestamp)*60 + lubridate::minute(dat$timestamp)
    # The Julian day
    dat$date <- as.Date(dat$timestamp)

    #### Define matrix
    if(verbose) cat("Step 3: Creating matrix of time (minutes) x time (days)...\n")
    rows <- seq(0, 24*60, by = res)
    cols <- seq.Date(min(dat$date), max(dat$date), by = 1)
    mat <- matrix(NA, nrow = length(rows), ncol = length(cols))
    rownames(mat) <- t1_units(rows)
    colnames(mat) <- t2_units(cols)
    # colnames(mat) <- t2_units(cols)
    for(i in 1:ncol(mat)){
      dat_on_day <- dat[dat$date == cols[i], ]
      mat[, i] <- dat_on_day$response[match(rows, dat_on_day$min)]
    }

    #### Visualise matrix
    if(verbose) cat("Step 4: Plotting matrix of time (minutes) x time (days)...\n")
    pretty_mat(mat,
               retain_orientation = retain_orientation,
               xlab = xlab,
               ylab = ylab,
               xtick_every_n = xtick_every_n,
               ytick_every_n = ytick_every_n,...)

    #### Return matrix
    if(return_mat) return(mat)

  }
