#' @title Define pretty limits and axes for publication-quality plots
#' @description This function is used to define pretty limits and axes on plots. The function can handle numeric, time stamp (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}) or factor data. Usually, arguments are passed from a plotting function (e.g., \code{\link[prettyGraphics]{pretty_plot}}) to this function via \code{pretty_axis_args}, although it can be called directly too. In the latter case, generally, the best approach is to implement the function prior to creating a plot. Based on the data to be plotted, the function defines axes limits and corresponding 'pretty' axis tick marks and labels, returning a list of outputs. Then, a plot can be created using limits defined by the function, after which point axes can be added to the plot by passing the list back to the function. Axis limits, placement, the number of ticks, labels and other axes properties can be determined automatically (in which case the function tries hard to create 'pretty' axes), adjusted (e.g. via adjustments to the number of 'pretty' breaks) or user-defined (e.g. by specifying axis breaks). Each axis can be controlled independently (e.g., one axis can be user-defined while another axis can be determined automatically and the function defines appropriate limits and axis placement). The function is very flexible (see Examples).
#'
#' @param side A numeric input specifying the side(s) of a plot for which pretty axes should be defined.
#' @param x A list, with one element for each side, defining the values to be plotted on that side of the plot. Numeric, time stamp (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}) or factor data are supported. Character vectors will be converted to factors for plotting.
#' @param lim (optional) A list, with one element for each side, containing a vector of (one or both) axes limits for that axis. If provided, then axis tick marks (pretty or regular) are forced to lie within provided limits. Otherwise, suitable limits can be suggested by the function based on the data provided in \code{x}. It is possible to fix only the lower or upper limit by specifying \code{c(user_specified_limit, NA)} to fix the first limit (or simply or \code{user_specified_limit} in which case the first limit is taken as the one that should be fixed), or \code{c(NA, user_specified_limit)} to fix the upper limit; the other limit is then chosen automatically depending on the inputs to other function arguments. For factors, user-supplied limits are ignored. For factors with one level, limits are set to 0.75 and 1.25; for factors with multiple levels, limits are set to 1 and the number of factor levels. However, factor limits can be adjusted by \code{control_factor_lim} (see below).
#' @param pretty A named list arguments that are used to create pretty axis tick marks. This list is passed to \code{\link[base]{pretty}} (for numeric data), \code{\link[lubridate]{pretty_dates}} (for time stamp data) or to an internal function (for factors) to create pretty axes. If \code{pretty = list()}, pretty sequences for an axis/axes are not created and a user-defined sequence is implemented instead (see below). If each axis should be controlled by the same pretty parameters, these can be specified in the pretty argument in a single list. If each axis should be controlled by different parameters, a nested list is required, with a list of arguments for each axis provided within the overall list (see Examples). The default option is to create pretty axes with approximately \code{n = 5} breaks. For factors, the only implemented argument is \code{n}, which defines the number of pretty breaks; any other supplied arguments are silently ignored.
#' @param units (optional) A list of units for each side. If \code{pretty = list()}, then a regular sequence of values between axes limits will be defined. This can be controlled by supplying the distance between sequential values to this argument (otherwise, a default value is used). For numeric axes, this is a number; for POSIXct axes, this is a character which specifies the duration between sequential ticks (e.g. "secs").
#' @param axis (optional) A named list of arguments that are supplied to \code{\link[graphics]{axis}}, \code{\link[graphics]{axis.POSIXct}} or \code{\link[graphics]{axis.Date}} that control axes (e.g. \code{cex.axis}, \code{pos}, \code{col}, etc.). As for the \code{pretty} argument, a single list of arguments will affect all axes; otherwise, a nested list can be provided so that each axis can be controlled independently (see Examples).
#' @param pi_notation (optional) A named list of arguments, passed to \code{\link[prettyGraphics]{pi_notation}}, to implement \eqn{\pi} notation for axis labels. The default option (\code{NULL}) suppresses this argument; a single list (\code{list()}) implements \eqn{\pi} notation for all axes using default arguments; and a nested list (e.g., \code{list(list(), NULL)}) implements \eqn{\pi} notation for specific axes.
#' @param control_axis (optional) A named list of arguments that affect all axes. This is only useful if a nested list is provided to \code{axis} (see above). In this case, any arguments that should affect all axes can be provided via \code{control_axis} so that these do not need to be provided to each list in \code{axis}. (This is simply for convenience.)
#' @param control_sci_notation A named list of arguments that controls scientific notation (see \code{\link[prettyGraphics]{sci_notation}}).
#' @param control_digits (optional) An integer which defines the number of decimal places on numeric axes. If \code{NULL}, the number of decimal places is set automatically. This argument affects all numeric axes, unless (a) axis labels are all integers; (b) \code{\link[prettyGraphics]{pretty_axis}} is implemented via \code{\link[prettyGraphics]{pretty_plot}} and \code{x} is plotted against an index; or (c) \code{\link[prettyGraphics]{sci_notation}} is implemented in which case decimal places should be controlled independently for axes with scientific notation via the \code{digits} argument in \code{control_sci_notation} (see \code{\link[prettyGraphics]{sci_notation}}).
#' @param control_factor_lim (optional) A number which specifies an additive adjustment to limits for a factor axis. For factors, with one or more level, limits become (0.75 - \code{control_factor_lim}) and (1.25 + \code{control_factor_lim}) or (1 - \code{control_factor_lim}) and (the number of factor levels + \code{control_factor_lim}) respectively.
#' @param axis_ls (optional) The output of a call to \code{\link[prettyGraphics]{pretty_axis}}. If this is provided, the function skips the definition of axis parameters and simply adds axes to a plot (see \code{add} below).
#' @param add A logical input specifying whether or not to plot the axes. Usually, prettier plots result when \code{\link[prettyGraphics]{pretty_axis}} is called prior to plotting to define axis limits; then, the plot can be created with those limits; and then the list created by the first call to \code{\link[prettyGraphics]{pretty_axis}} can be supplied to the function again via the \code{axis_ls} argument, with \code{add = TRUE}, to add the axes (see Examples).
#' @param return_list (depreciated) A logical input defining whether or not to return a list of axis parameters defined by the function.
#' @param ... Arguments (\code{cex.axis, cex.lab, col.axis, col.lab, font.axis, font.lab, las}) passed from other methods that are added to the \code{control_axis} argument and affect all axes.
#'
#' @return The function returns a list of parameters with an element for each \code{side}. Each element is a list which contains two elements: 'lim', a vector of the lower and upper axis limits for that \code{side} and 'axis', a list of parameters that define the axis for that \code{side}.
#'
#' @author Edward Lavender
#' @export
#'
#' @examples
#' #### Generate some data for numeric examples
#' set.seed(1)
#' x <- 1:10
#' y <- rnorm(length(x), x*5-10, 5)
#'
#' #### The default options
#' # The default options define the parameters of pretty axes with approx. n = 5 breaks
#' # apply pretty_axis to generate a list of parameters for pretty axes;
#' # we'll use the outputs to set the limits of the graph and then add pretty axes:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               pretty = list(n = 5),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' # plot the graph using limits defined by function
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' # add pretty axes by passing the axis_args list back to the function
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Adjusting pretty axes via the pretty argument
#' # We can add arguments to the pretty list that are passed to pretty() or lubridate::pretty_dates()
#' # ... to adjust the pretty axes produced.
#' # ... For example, we can adjust the minimum number of intervals on all axes if required:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               pretty = list(n = 5, min.n = 5),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Adjusting different axes differently
#' # In example 1 and 2, the changes to the pretty argument affect all axes
#' # ... added to all specified sides. To make changes side-specific,
#' # ... we need to specify the arguments for each side in a separate list.
#' # For example, to have approx. 5 breaks on the first axis and 20 breaks on the second:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               pretty = list(list(n = 5), list(n = 20)),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Pretty labels can be constrained within limits:
#' # Pretty labels are forced to be within limits if these are specified by the user
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-3, 15), y = c(-19, 49)),
#'               pretty = list(n = 5),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' # Axes limits can be specified for some axes but not others by suppling
#' # ... NULL elements to the lim list, as follows:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(NULL, c(-19, 49)),
#'               pretty = list(n = 5),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### For any axis, only one limit can be fixed and the other will be define internally:
#' # If only one limit is provided, this is assumed to be the first limit:
#' pretty_axis(side = 1:2, x = list(x, y), lim = list(0, NULL))[[1]]$lim
#' # The second limit can be coded explicitly as NA:
#' pretty_axis(side = 1:2, x = list(x, y), lim = list(c(0, NA), NULL))[[1]]$lim
#' # To hard-code the second limit, you must specify the first limit as NA:
#' pretty_axis(side = 1:2, x = list(x, y), lim = list(c(0, NA), NULL))[[1]]$lim
#' # Times work in the same way (see also later exampes):
#' pretty_axis(side = 1:2,
#'             x = list(as.Date(c("2016-01-04", "2016-05-01")), 1:2),
#'             lim = list(as.Date("2016-01-01"), NULL))[[1]]$lim
#' pretty_axis(side = 1:2,
#'             x = list(as.POSIXct(c("2016-01-04", "2016-05-01")), 1:2),
#'             lim = list(as.POSIXct("2016-01-01"), NULL))[[1]]$lim
#'
#' #### We can create regular sequences instead of pretty ones
#' # Instead of creating 'pretty' axes, we can choose to create a regular sequence
#' # ... and specify the total number of units between the start and end points
#' # ... (if we don't specify this, the default is 5 units or
#' # ... an automatically determined number of time steps, see below).
#' # Note that because units only takes one argument for each axes,
#' # ... we do not specify a nested list like we do for the pretty argument
#' # ... (or, as we'll see below) for the axis argument,
#' # ... in which all the arguments for each side need to be grouped into a list.
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               units = list(5, 3),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### More on controlling each axis separately
#' # Of course, we can have some axes with pretty axes and others with user defined units
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(), list(n = 5)),
#'               units = list(5, NULL),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Arguments are passed to axis(), axis.POSIXct() or axis.Date() via the axis argument
#' # As above, if we supply these once they will affect all graphs:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(), list(n = 5)),
#'               units = list(5, NULL),
#'               axis = list(col = "red", cex.axis = 1.5),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Graphical properties of each axis can be controlled separately
#' # We can change individual axes by creating a list of arguments for each axis
#' # changes to individual axes need to be specified via individual lists;
#' # e.g. to make the first axis red and the second blue, and to have 10 pretty labels
#' # ... on the first axis and 5 on the second we need to:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(n = 10), list(n = 5)),
#'               units = list(),
#'               axis = list(list(col = "blue", cex.axis = 1.5), list(col = "red", cex.axis = 1.5)),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Shared arguments for each axis (e.g., cex.axis) can be passed via control_axis
#' # ... list to reduce typing in cases where other axis parameters require separate control
#' # ... (i.e., to avoid having to pass these arguments to every list in a nested list).
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(n = 10), list(n = 5)),
#'               units = list(),
#'               axis = list(list(col = "blue"), list(col = "red")),
#'               control_axis = list(cex.axis = 1.5),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' axis_args[[1]]$axis$cex.axis
#' axis_args[[2]]$axis$cex.axis
#'
#' #### Control the number of decimal places for numeric axes via control_digits
#' # axis label decimal places are chosen automatically with the default setting control_digits = NULL:
#' axis_ls <- pretty_axis(side = 1, x = list(seq(0, 1, by = 0.1)), control_digits = 1)
#' axis_ls[[1]]$axis$labels
#' # user-specified decimal places:
#' axis_ls <- pretty_axis(side = 1, x = list(seq(0, 1, by = 0.1)), control_digits = 3)
#' axis_ls[[1]]$axis$labels
#'
#' #### Generate time stamp data
#' # Generate some x and y values, where x values are time stamps
#' # ... in POSIXct format. Note the incorporation of tz.
#' x <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"),
#'                 as.POSIXct("2016-01-02", tz = "UTC"), by = "2 mins")
#' y <- rnorm(length(x), as.numeric(x)*1e-6 + 100, 50)
#'
#' #### We can use this function with time stamps in POSIXct format too.
#' # Apply pretty_axis() function prior to plot to obtain suitable limits:
#' axis_args <-
#'   pretty_axis(side = 1:4,
#'               x = list(x, y),
#'               lim = list(),
#'               pretty = list(n = 5),
#'               axis = list(list(),
#'                           list(las = TRUE),
#'                           list(labels = FALSE),
#'                           list(labels = FALSE)),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' # Plot graph using pretty_axis() limits:
#' plot(x, y,
#'      type = "l",
#'      axes = FALSE,
#'      xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim
#' )
#' # Add pretty axes by passing the list of axis_args back to the function
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Axis parameters for time stamps are passed to axis.POSIXct() or axis.Date()
#' # ... which can incorporate other options
#' axis_args <-
#'   pretty_axis(side = 1:4,
#'               x = list(x, y),
#'               lim = list(),
#'               pretty = list(n = 5),
#'               axis = list(list(format = "%H"),
#'                            list(las = TRUE),
#'                            list(labels = FALSE),
#'                            list(labels = FALSE)),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y,
#'      type = "l",
#'      axes = FALSE,
#'      xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Regular sequences, instead of 'pretty' axes can be generated with dates too
#' # To do this, units takes a character input (e.g. "mins" or "hours")
#' axis_args <-
#'   pretty_axis(side = 1:4,
#'               x = list(x, y),
#'               lim = list(),
#'               pretty = list(list(), list(n = 3)),
#'               units = list("hours", NULL),
#'               axis = list(list(format = "%H"),
#'                           list(las = TRUE),
#'                           list(labels = FALSE),
#'                           list(labels = FALSE)),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y,
#'      type = "l",
#'      axes = FALSE,
#'      xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### For factors, pretty axes can be created via pretty or via units.
#' # .. If pretty is supplied, approximately n factor levels are retained:
#' # ... and corresponding labels are added by default.
#' # Example data:
#' dx <- factor(LETTERS[1:10])
#' dy <- 1:10
#' # Example with tick for every level:
#' axis_ls <-
#'   pretty_axis(side = 1:2,
#'               x = list(dx, dy),
#'               pretty = list(n = length(dx)),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' axis_ls[[1]]
#' # Note that x limits are automatically defined between 1 and the maximum number of factors:
#' axis_ls[[1]]; length(levels(dx))
#' # However, default factor limits can be extended via control_factor_lim
#' axis_ls <-
#'   pretty_axis(side = 1:2,
#'               x = list(dx, dy),
#'               pretty = list(n = length(dx)),
#'               control_factor_lim = 0.5,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' axis_ls[[1]]$lim
#' # Example with tick mark for every other level
#' axis_ls <-
#'   pretty_axis(side = 1:2,
#'               x = list(dx, dy),
#'               pretty = list(n = length(dx)/2),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' axis_ls[[1]]
#'
#' #### For factors, pretty axes can also be specified via units:
#' # For example, to select every factor level:
#' axis_ls <-
#'   pretty_axis(side = 1:2,
#'               x = list(dx, dy),
#'               pretty = list(list(), list(n = 5)),
#'               units = list(1, NULL),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' axis_ls[[1]]
#' # Or, to select every other factor level:
#' axis_ls <-
#'   pretty_axis(side = 1:2,
#'               x = list(dx, dy),
#'               pretty = list(list(), list(n = 5)),
#'               units = list(2, NULL),
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' axis_ls[[1]]
#'
#' #### Examples with pi notation
#' ## Define an example lunar phase time series
#' x <- seq.Date(as.Date("2016-01-01"), as.Date("2016-02-01"), by = 1)
#' y <- lunar::lunar.phase(x)
#' ## Under the default options, pi notation is suppressed
#' x <- 1:10
#' y <- 1:10
#' axis_ls <- pretty_axis(side = 1:2,
#'                        x = list(x, y),
#'                        pretty = list(n = 5),
#'                        return_list = TRUE
#'                        )
#' lapply(axis_ls, function(x) x$axis[c("at", "labels")])
#' ## To use pi notation for all axes with default args, specify list()
#' axis_ls <- pretty_axis(side = 1:2,
#'                        x = list(x, y),
#'                        pretty = list(n = 5),
#'                        pi_notation = list(),
#'                        return_list = TRUE
#'                        )
#' lapply(axis_ls, function(x) x$axis[c("at", "labels")])
#' ## To use pi notation for specific axes, specify a nested list():
#' axis_ls <- pretty_axis(side = 1:2,
#'                        x = list(x, y),
#'                        pretty = list(n = 5),
#'                        pi_notation = list(NULL, list()),
#'                        return_list = TRUE
#'                        )
#' lapply(axis_ls, function(x) x$axis[c("at", "labels")])
#' ## Pass arguments to pi_notation() to customise the result
#' axis_ls <- pretty_axis(side = 1:2,
#'                        x = list(x, y),
#'                        pretty = list(n = 5),
#'                        pi_notation = list(NULL, list(as_fraction = FALSE))
#'                        )
#' lapply(axis_ls, function(x) x$axis[c("at", "labels")])
#'
#' #### The influence of NAs
#' # For all data types, NAs are removed with a message:
#' pretty_axis(x = list(factor(c(1, 2, NA)), 1:3))
#'

pretty_axis <-
  function(side = 1:4,
           x = list(),
           lim = list(),
           pretty = list(n = 5),
           units = list(),
           axis = list(),
           pi_notation = NULL,
           control_axis = list(las = TRUE),
           control_sci_notation = list(),
           control_digits = NULL,
           control_factor_lim = 0.5,
           axis_ls = NULL,
           add = FALSE,
           return_list = TRUE,...
  ){



    ##############################################
    ##############################################
    #### Define axis parameters

    #### If axis parameters have not been supplied...
    if(is.null(axis_ls)){


      ##############################################
      #### Checks

      #### Check that sides are provided in the correct order
      if(length(side) == 3) stop("Three axis sides are not currently supported.")
      if(length(side) > 1){
        if(!(side[1] %in% c(1, 3))) stop("side[1] should refer to an x axis (i.e., side[1] = 1 or 3).")
        if(!(side[2] %in% c(2, 4))) stop("side[2] should refer to a y axis (i.e., side[2] = 2 or 4).")
      }

      #### Extract x values from lim, if not supplied.
      if(length(x) == 0){
        if(length(lim) == 0){
          stop("'x' or 'lim' must be supplied.")
        } else{
          message("pretty_axis() 'x' values taken from 'lim'.")
          x <- lim
        }
      }

      #### Check argument class is "list", where required, and convert empty lists to list(NULL).
      # This is required to loop over lists correctly, but it is more intuitive for the user to specify
      # ... list() rather than list(NULL) to blank out an option.
      check_input_class(arg = "x", input = x, if_class = NULL, to_class = "list", type = "stop")
      lim <- empty_list_to_list_null("lim", lim)
      pretty <- empty_list_to_list_null("pretty", pretty)
      units  <- empty_list_to_list_null("units", units)
      axis   <- empty_list_to_list_null("axis", axis)
      if(is.null(pi_notation)) pi_notation <- lapply(1:length(side), function(x) -1)
      pi_notation <- empty_list_to_list_null("pi_notation", pi_notation)

      #### Check the length of lists is correct.
      mapply(list(x, lim, units),
             list("x", "lim", "units"),
             FUN = function(elm, elm_name){
               if(length(elm) > 0 & length(elm) > length(side)) {
                 stop(paste0(length(side), " side(s) supplied but argument '", elm_name, "' contains ", length(elm), " elements."))
               }
             })

      #### Check data types
      # Check data
      x <- mapply(x, 1:length(x), FUN = function(elm, i){
        # Define argument name
        arg <- paste0("x[[", i, "]]")
        #  If a character is supplied, convert to factor, with a warning:
        elm <- check_input_class(arg = arg, input = elm,
                                 if_class = "character", to_class = "factor",
                                 type = "warning", coerce_input = factor)
        # If a time stamp if supplied, ensure a tz is included:
        elm <- check_tz(arg, elm)
        return(elm)
      }, SIMPLIFY = FALSE)
      # Repeat for limits; we only need to check time stamps here because factor limits are ignored.
      lim <- mapply(lim, 1:length(lim), FUN = function(elm, i){
        # Define argument name
        arg <- paste0("lim[[", i, "]]")
        # If a time stamp if supplied, ensure a tz is included:
        elm <- check_tz(arg, elm)
        return(elm)
      }, SIMPLIFY = FALSE)

      #### Explicit consideration of NAs in data.
      # This is generally useful, but necessary to stop errors with factor levels that are NA.
      # Check the number of observations in each element of x is the same:
      lx <- sapply(x, length)
      if(length(unique(lx)) != 1){
        message("'x' contains elements with different numbers of observations; collapsing each element (i) in 'x' to range(i).")
        x <- lapply(x, function(e) {
          if(inherits(e, "factor")){
            rng <- range_factor(e)
          } else {
            rng <- range(e, na.rm = TRUE)
          }
           return(rng)
        })
        lx <- c(2, 2)
      }
      # Create a dataframe and drop NAs
      dat <- data.frame(do.call(cbind, x))
      dat <- dat[stats::complete.cases(dat), , drop = FALSE]
      # Check whether there are NAs and, if so, drop these.
      nrw <- nrow(dat)
      if(nrw != lx[1]){
        lna <- lx[1] - nrw
        message(paste(lna, "observation pair(s) in x are NA; these are removed."))
        x <- lapply(1:ncol(dat), function(i) return(dat[, i]))
        if(length(x[[1]]) < 1) stop("No non NA observations left in x.")
      }

      #### Adjust lists if necessary, to ensure mapply() loops over lists correctly.
      lim    <- list_adjust(l = lim, f = length, side = side)
      pretty <- list_adjust(l = pretty, f = list_depth, side = side)
      units  <- list_adjust(l = units, f = length, side = side)
      axis   <- list_adjust(l = axis, f = list_depth, side = side)
      pi_notation <- list_adjust(l = pi_notation, f = list_depth, side = side)

      #### Update control axis
      dots <- list(...)
      dnms <- names(dots)
      if(!is.null(dnms)){
        for(param in c("cex.axis", "cex.lab",
                       "col.axis", "col.lab",
                       "font.axis", "font.lab",
                       "las")){
          if(param %in% dnms) control_axis[[param]] <- dots[[param]]
        }
      }


      ##############################################
      #### Define axis ls

      axis_ls <-
        mapply(side, x, lim, pretty, units, axis, pi_notation,
               SIMPLIFY = FALSE,
               FUN = function(iside, ix, ilim, ipretty, iunits, iaxis, ipi){


          ##############################################
          #### Processing

          #### Testing
          testing <- FALSE
          if(testing){
            iside <- side[1]
            ix <- x[[1]]
            ilim <- lim[[1]]
            ipretty <- pretty[[1]]
            iunits <- units[[1]]
            iaxis <- axis[[1]]
            ipi <- pi_notation[[1]]
          }

          #### Compact lists
          # unlist limits if necessary
          ilim <- unlist(ilim)
          # the default is a blank list (list(NULL))
          # this is necessary for mapply, but causes problems down the line
          # so, having passed the arguments to mapply, we'll now remove NULLs:
          ipretty <- compact(ipretty)
          iunits  <- unlist(compact(iunits)) # unlist if necessary (if only one side supplied).
          iaxis   <- compact(iaxis)
          ipi     <- compact(ipi)
          ipi     <- compact(ipi)
          if(all(sapply(1:length(ipi), function(i) isTRUE(unlist(ipi)[i] == -1)))) ipi <- NULL

          #### Scale ix
          if(is_number(ix)) {
            if(!is.null(ipi)) {
              ix <- ix/pi
              if(!is.null(ilim)) ilim <- ilim/pi
            }
          } else {
            if(!is.null(ipi)) warning("pi_notation argument(s) ignored for non-numeric variable(s).")
          }

          #### Check units
          # Down the line, if pretty is implemented as well as units
          # ... ie., units != list() then we'll give a warning.
          # This is necessary because pretty = list(n = 5) is implemented by default and remains
          # ...implemented by default even if units are supplied.
          iunits_warn <- ifelse(length(iunits) != 0, TRUE, FALSE)
          iunits <- units_x(iunits, x = ix)


          ##############################################
          #### Define axis limits and tick mark positions

          #### Define initial axis limits
          ilim <- define_lim_init(x = ix, at = iaxis$at, lim = ilim)

          #### Define x axis positions
          # If positions have not been provided...
          if(is.null(iaxis$at)){

            #### Option (a) A pretty sequence
            # If the user has requested pretty sequence...
            if(length(ipretty) > 0){

              #### Check if units have been supplied, if so, print a warning that these are ignored
              if(iunits_warn) warning("Both pretty and units specified for an axis. pretty arguments implemented.")

              #### Define pretty axis positions and limits
              pretty_seq_ls <- pretty_seq(x = ix, lim = ilim, pretty_args = ipretty)
              iaxis$at      <- pretty_seq_ls$at
              ilim          <- pretty_seq_ls$lim

            #### Option (b) A regular sequence between limits
            } else{
              iaxis$at <- seq_x(ilim[1], ilim[2], units = iunits)
            }
          }

          #### Post-processing controls
          # Limits can be expanded for factors by an amount specified by control_factor_lim
          # This step needs to be implemented after defining tick marks, else tick marks
          # ... are defined within expanded limits
          if(is.factor(ix)){
            ilim <- c(min(ilim) - control_factor_lim, max(ilim) + control_factor_lim)
            attributes(ilim)$user <- c(FALSE, FALSE)
          }


          ##############################################
          #### Define other axis list components

          #### Define pretty labels
          if(is.null(iaxis$labels)) iaxis$labels <- pretty_labels(x = ix,
                                                                  at = iaxis$at,
                                                                  n = control_digits,
                                                                  pi_notation_args = ipi,
                                                                  sci_notation_args = control_sci_notation)

          #### Re-scale 'at' and 'ilim' for (pi)_notation
          if(is_number(ix) & !is.null(ipi)){
            ilim <- ilim * pi
            iaxis$at <- iaxis$at * pi
          }

          #### Other properties
          iaxis$side <- iside
          if(length(control_axis) > 0) iaxis <- rlist::list.merge(iaxis, control_axis)

          #### Return ixaxis and other parameters
          out <- list(axis = iaxis, lim = ilim)
          return(out)

        }) # close mapply(

      #### List names
      names(axis_ls) <- side


      ##############################################
      #### Define axis positions

      #### Axis positioning
      # position of axis one side = 1 is based on min of side 2 or 4
      # position of axis on side = 2 is based on min of side 1 or 3
      # position of axis on side = 3 is based on max of side 2 or 4
      # position of axis on side = 4 is based on max of side 1 or 3
      s1s <- c("1", "2", "3", "4")
      s2s <- list(c("2", "4"), c("1", "3"), c("2", "4"), c("1", "3"))
      sfs <- list(min, min, max, max)
      for(s in 1:4){
        s1 <- s1s[s]
        s2 <- s2s[[s]]
        s21 <- s2[1]
        s22 <- s2[2]
        sf <- sfs[[s]]
        if(s1 %in% side){
          if(is.null(axis_ls[[s1]]$axis$pos) & !is.null(axis_ls[[s21]]$axis) | !is.null(axis_ls[[s22]]$axis)){
            s2select <- s2[which(c(!is.null(axis_ls[[s21]]$axis), !is.null(axis_ls[[s22]]$axis)))]
            s2select <- s2select[1]
            axis_ls[[s1]]$axis$pos <- sf(axis_ls[[s2select]]$lim)
          }
        }
      }
    }

    #### Check names do not contain NA,
    # This can occur if the wrong number of elements is supplied
    # .. given the number of sides.
    if(any(is.na(names(axis_ls)))){
      stop("names(axis_ls) contains NA(s). The number of sides and the number of elements in argument lists is not aligned.")
    }


    ##############################################
    ##############################################
    #### Add axis if requested

    if(add){
      # loop over every element in axis_ls and add using user-provided parameters
      lapply(axis_ls, function(elem){
        # First, plot a full line across the limits
        # This means if the user has specified limits then any pretty labels
        # ... will be shown inside these limits, with the full extent of the graph properly shown.
        tmp_axis_args <- elem$axis
        tmp_axis_args$at <- elem$lim
        tmp_axis_args$lwd.ticks <- c(0, 0)
        tmp_axis_args$labels <- c("", "")
        # Define at
        at <- elem$axis$at
        # Define appropriate function
        add_axis <- choose_foo_axis(at)
        do.call(add_axis, tmp_axis_args)
        do.call(add_axis, elem$axis)
      })
    }


    #### Return pretty axis list, if requested.
    if(return_list) return(axis_ls)

  }



#### End of code.
##############################################
##############################################
