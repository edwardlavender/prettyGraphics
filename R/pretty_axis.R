#' @title Define pretty limits and axes for publication quality plots
#' @description This function is used to define pretty limits and axes on plots. The function can handle numeric, timestamp (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}) or factor data. In most cases, the best approach is to implement the function prior to creating a plot. Based on the data to be plotted, the function defines axes limits and corresponding 'pretty' axis labels, returning a list of outputs. Then, a plot can be created using limits defined by the function, after which point axes can be added to the plot by passing the list back to the function. Axis limits, placement, the number of ticks, labels and other axes properties can be determined automatically (in which case the function tries hard to create 'pretty' axes), adjusted (e.g. via adjustments to the number of 'pretty' breaks) or user-defined (e.g. by specifying axis breaks). Each axis can be controlled independently (e.g., one axis can be user-defined while another axis can be determined automatically and the function defines appropriate limits and axis placement). The function is very flexible (see Examples).
#'
#' @param side A numeric input specifying the side(s) of a plot for which pretty axes should be defined.
#' @param x A list, with one element for each side, defining the values to be plotted on that side of the plot. Numeric, timestamp (i.e. \code{\link[base]{Dates}} or \code{\link[base]{DateTimeClasses}}) or factor data are supported. Character vectors will be converted to factors for plotting.
#' @param lim (optional) A list, with one element for each side, containing a vector of axes limits for that axis. If provided, then axes limits (pretty or regular) are forced to lie within provided limits. Otherwise, suitable limits can be suggested by the function based on the data provided in \code{x}. For factors, these limits are (1, number of factor levels).
#' @param pretty A list of named arguments to be provided to \code{\link[base]{pretty}} (for numeric data), \code{\link[lubridate]{pretty_dates}} (for timestamp data) or an internal approach (for factors) to create pretty axes. If \code{pretty = list(NULL)}, pretty sequences for an axis/axes are not created and a user-defined sequence is implemented instead (see below). If each axis should be controlled by the same pretty parameters, these can be specified in the pretty argument in a single list. If each axis should be controlled by different parameters, a nested list is required, with a list of arguments for each axis provided within the overall list (see Examples). The default option is to create pretty axes with approximately \code{n = 5} breaks. For factors, the only implemented argument is \code{n}; any other supplied arguments are silently ignored.
#' @param units (optional) A list of units for each side. If \code{pretty = list(NULL)}, then a regular sequence of values between axes limits will be defined. This can be controlled by supplying the distance between sequential values to this argument (otherwise, a default value is used). For numeric axes, this is a number; for POSIXct axes, this is a character which specifies the duration between sequential ticks (e.g. "secs").
#' @param axis (optional) A list of arguments that are supplied to \code{\link[graphics]{axis}}, \code{\link[graphics]{axis.POSIXct}} or \code{\link[graphics]{axis.Date}} that control axes (e.g. \code{cex.axis}, \code{pos}, \code{col}, etc.). As for the \code{pretty} argument, a single list of arguments will affect all axes; otherwise, a nested list can be provided so that each axis can be controlled independently (see Examples).
#' @param axis_ls (optional) The output of a call to \code{pretty_axis()}. If this is provided, the function skips the definition of axis parameters and simply adds axes to a plot (see \code{add} below).
#' @param add A logical input specifying whether or not to plot the axes. Usually, prettier plots result when \code{pretty_axis()} is called prior to plotting to define axis limits; then, the plot can be created with those limits; and then the list created by the first call to \code{pretty_axis()} can be supplied to the function again via the \code{axis_ls} argument, with \code{add = TRUE}, to add the axes (see Examples).
#' @param return_list A logical input defining whether or not to return a list of axis parameters defined by the function.
#'
#' @return The function returns a list of parameters with an element for each \code{side}. Each element is a list which contains two elements: 'lim', a vector of the lower and upper axis limits for that \code{side} and 'axis', a list of parameters that define the axis for that \code{side}.
#'
#' @author Edward Lavender
#' @export
#'
#' @examples
#'
#' #### Generate some data for numeric examples
#' set.seed(1)
#' x <- 1:10
#' y <- rnorm(length(x), x*5-10, 5)
#'
#' #### Example 1: The default options
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
#' #### Example 2: Adjusting pretty axes via the pretty argument
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
#' #### Example 3: Adjusting different axes differently
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
#' #### Example 4: Pretty labels can be constrained within limits:
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
#' #### Example 5: We can create regular sequences instead of pretty ones
#' # Instead of creating 'pretty' axes, we can choose to create a regular sequence
#' # ... and specify the total number of units between the start and end points
#' # ... (if we don't specify this, the default is 5 units or
#' # ... an automatically determined number of timesteps, see below).
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
#' #### Example 6: More on controlling each axis separately
#' # Of course, we can have some axes with pretty axes and others with user defined units
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(NULL), list(n = 5)),
#'               units = list(5, NULL),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Example 7: Arguments are passed to axis(), axis.POSIXct() or axis.Date() via the axis argument
#' # As above, if we supply these once they will affect all graphs:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(NULL), list(n = 5)),
#'               units = list(5, NULL),
#'               axis = list(col = "red", cex.axis = 1.5),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Example 8: Graphical properties of each axis can be controlled separately
#' # We can change individual axes by creating a list of arguments for each axis
#' # changes to individual axes need to be specified via individual lists;
#' # e.g. to make the first axis red and the second blue, and to have 10 pretty labels
#' # ... on the first axis and 5 on the second we need to:
#' axis_args <-
#'   pretty_axis(side = 1:2,
#'               x = list(x, y),
#'               lim = list(x = c(-2, 12), y = c(-10, 41)),
#'               pretty = list(list(n = 10), list(n = 5)),
#'               units = list(NULL),
#'               axis = list(list(col = "blue", cex.axis = 1.5), list(col = "red", cex.axis = 1.5)),
#'               axis_ls = NULL,
#'               add = FALSE,
#'               return_list = TRUE
#'   )
#' plot(x, y, axes = FALSE, xlim = axis_args$`1`$lim, ylim = axis_args$`2`$lim)
#' pretty_axis(axis_ls = axis_args, add = TRUE)
#'
#' #### Generate timestamp data
#' # Generate some x and y values, where x values are timestamps
#' # ... in POSIXct format. Note the incorporation of tz.
#' x <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"),
#'                 as.POSIXct("2016-01-02", tz = "UTC"), by = "2 mins")
#' y <- rnorm(length(x), as.numeric(x)*1e-6 + 100, 50)
#'
#' #### Example 9: We can use this function with timestamps in POSIXct format too.
#' # Apply pretty_axis() function prior to plot to obtain suitable limits:
#' axis_args <-
#'   pretty_axis(side = 1:4,
#'               x = list(x, y),
#'               lim = list(NULL),
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
#' #### Example 10: axis parameters for timestamps are passed to axis.POSIXct() or axis.Date()
#' # ... which can incorporate other options
#' axis_args <-
#'   pretty_axis(side = 1:4,
#'               x = list(x, y),
#'               lim = list(NULL),
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
#' #### Example 11: Regular sequences, instead of 'pretty' axes can be generated with dates too
#' # To do this, units takes a character input (e.g. "mins" or "hours")
#' axis_args <-
#'   pretty_axis(side = 1:4,
#'               x = list(x, y),
#'               lim = list(NULL),
#'               pretty = list(list(NULL), list(n = 3)),
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
#' #### Example (12): For factors, pretty axes can be created via pretty or via units.
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
#' #### Example (13): For factors, pretty axes can also be specified via units:
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

##############################################
##############################################
#### pretty_axis

pretty_axis <-
  function(side = 1:4,
           x = list(),
           lim = list(NULL),
           pretty = list(n = 5),
           units = list(NULL),
           axis = list(NULL),
           axis_ls = NULL,
           add = FALSE,
           return_list = TRUE
  ){



    ##############################################
    ##############################################
    #### Define axis parameters

    #### If axis parameters have not been supplied...
    if(is.null(axis_ls)){


      ##############################################
      #### Checks

      #### Check that sides are provided in the correct order
      # ...

      #### Check that data are supplied
      stopifnot(!(length(x) == 0))

      #### Check argument class is "list", where required.
      mapply(c("x", "lim", "pretty", "units", "axis"),
             list(x, lim, pretty, units, axis),
             FUN = function(arg, elm){
               check_input_class(arg = arg, input = elm, if_class = NULL, to_class = "list", type = "stop")
               })

      #### Check the length of lists is correct.
      mapply(list(x, lim, units),
             list("x", "lim", "units"),
             FUN = function(elm, elm_name){
               if(length(elm) > 0 & length(elm) > length(side)) {
                 stop(paste0(length(side), " side(s) supplied but argument '", elm_name, "' contains ", length(elm), " elements."))
               }
             })

      #### Check data types
      if(length(x) > 0){
        x <- mapply(x, 1:length(x), FUN = function(elm, i){
          # Define argument name
          arg <- paste0("x[[", i, "]]")
          #  If a character is supplied, convert to factor, with a warning:
          elm <- check_input_class(arg = arg, input = elm,
                                   if_class = "character", to_class = "factor",
                                   type = "warning", coerce_input = factor)
          # If a timestamp if supplied, ensure a tz is included:
          elm <- check_tz(arg, elm)
          return(elm)
        }, SIMPLIFY = FALSE)
      }

      #### Adjust lists if necessary, to ensure mapply() loops over lists correctly.
      lim    <- list_adjust(l = lim, f = length, side = side)
      pretty <- list_adjust(l = pretty, f = plotrix::listDepth, side = side)
      units  <- list_adjust(l = units, f = length, side = side)
      axis   <- list_adjust(l = axis, f = plotrix::listDepth, side = side)


      ##############################################
      #### Define axis ls

      axis_ls <-
        mapply(side, x, lim, pretty, units, axis,
               SIMPLIFY = FALSE,
               FUN = function(iside, ix, ilim, ipretty, iunits, iaxis){


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
          }

          #### Compact lists
          # unlist limits if necessary
          ilim <- unlist(ilim)
          # the default is a blank list (list(NULL))
          # this is necessary for mapply, but causes problems down the line
          # so, having passed the arguments to mapply, we'll now remove NULLs:
          ipretty <- plyr::compact(ipretty)
          iunits <- unlist(plyr::compact(iunits)) # unlist if necessary (if only one side supplied).
          iaxis <- plyr::compact(iaxis)

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


          ##############################################
          #### Define other axis list components

          #### Define pretty labels
          if(is.null(iaxis$labels)) iaxis$labels <- pretty_labels(x = ix, at = iaxis$at)

          #### Return ixaxis and other parameters
          iaxis$side <- iside
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
