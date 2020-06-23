###################################
###################################
#### is_number()

#' @title Check if an object is a number
#' @description This function checks if an input is a number (i.e., an integer or numeric object).
#' @param x An object.
#' @return A logical value which defines whether or not \code{x} is a number.
#' @author Edward Lavender
#' @keywords internal

is_number <-
  function(x){
    is_number <- inherits(x, "integer") | inherits(x, "numeric")
    return(is_number)
  }


###################################
###################################
#### is_time()

#' @title Check if an object is a time
#' @description This function checks if an input is a time (i.e., a POSIXct or Date object).
#' @param x An object.
#' @return A logical value which defines whether or not \code{x} is a time.
#' @author Edward Lavender
#' @keywords internal

is_time <-
  function(x){
    is_time <- inherits(x, "POSIXct") | inherits(x, "Date")
    return(is_time)
  }


###################################
###################################
#### units_x()

#' @title Define units within \code{\link[plot.pretty]{pretty_axis}}
#' @description This function defines suitable default units (i.e., distances between sequential observations in a regular sequence) for a given input type (numbers or times).
#' @param iunits A number or character. If code{NULL}, the function defines suitable units based on the object class (see \code{x} and Value). Otherwise, the function returns \code{iunits} unchanged.
#' @param x An object.
#' @return If \code{iunits} is \code{NULL}, the function returns units = 5 for number inputs and units = "auto" for time inputs. Otherwise, the function returns \code{iunits} as inputted.
#' @author Edward Lavender
#' @keywords internal

units_x <- function(iunits, x){
  if(length(iunits) == 0){
    if(is_time(x)){
      iunits <- "auto"
    } else{
      units <- 5
    }
  } else{
    return(iunits)
  }
}


###################################
###################################
#### diff_x()

#' @title Calculate a difference between two values
#' @param x2 A number or time.
#' @param x1 A number or time.
#' @return The function returns the (absolute) difference between \code{x1} and \code{x2}.
#' @author Edward Lavender
#' @keywords internal

diff_x <- function(x2, x1){
  if(is_number(x1)){
    d <- abs(x2 - x1)
  } else if(is_time(x1)){
    d <- difftime(x2, x1, units = "secs")
  }
  return(d)
}


###################################
###################################
#### seq_x()

#' @title Create a regular sequence between two values
#' @description This function creates a sequence between two values (numbers or times).
#' @param x1 The first number/time in the sequence.
#' @param x2 The last number/time in the sequence.
#' @param units The distance between sequential values in the sequence.
#' @return The function returns a regular sequence between two values.
#' @author Edward Lavender
#' @keywords internal

seq_x <- function(x1, x2, units){
  if(is_number(x1)){
    s <- seq(x1, x2, by = units)
  } else if(is_time(x1)){
    # If units are "auto", use difftime() to determine suitable units.
    if(units == "auto"){
      duration <- difftime(x1, x2, units = units)
      units <- attributes(duration)$units
    }
    s <- seq.POSIXt(x1, x2, by = units)
  }
  return(s)
}



###################################
###################################
#### pretty_x()

#' @title Create a regular pretty sequence
#' @description This function creates a regular pretty sequence from numbers or times.
#' @param x A vector of numbers or times.
#' @param ... Other arguments passed to \code{\link[base]{pretty}} or \code{\link[lubridate]{pretty_dates}}.
#' @return The function returns a series of pretty numbers or times.
#' @author Edward Lavender
#' @seealso \code{link[plot.pretty]{pretty_seq}} is an extension of this function.
#' @keywords internal

pretty_x <- function(x,...){

  #### Check variable types
  if(is.character(x)) x <- factor(x)

  #### Method for numbers
  if(is_number(x)){
    s <- pretty(x,...)

  #### Method for times
  } else if(is_time(x)){
    s <- lubridate::pretty_dates(x,...)
    # pretty_dates() converts objects to POSIXct so, we'll
    # convert back to a Date if necessary, so that axes are defined
    # on the same scale as the data
    if(inherits(x, "Date")) s <- as.Date(s)

  #### method for factors
  } else if(is.factor(x)){
    # generate a regular sequence...
    s <- seq(min(as.numeric(x), na.rm = TRUE), max(as.numeric(x), na.rm = TRUE),...)
  }

  #### Return sequence
  return(s)
}



###################################
###################################
#### define_lim_init()

#' @title Define initial limits from a sequence of numbers.
#' @description. This function defines initial limits for a sequence of numbers.
#' @param x A vector which contains user-supplied data.
#' @param at A vector which contains user-supplied axis positions.
#' @param lim A vector of length two which contains user-supplied axis positions.
#' @details If the user has supplied limits, this function checks limits to ensure that they are sensible, and flags them with a 'user' attribute (so that user-defined limits are recognisable and remain fixed). If the user has not supplied limits, limits are extracted from axis tick mark positions, if specified, or otherwise from the data provided.
#' @return This function returns a vector of two numbers representing initial limits.
#' @author Edward Lavender
#' @keywords internal

define_lim_init <-
  function(x,
           at = NULL,
           lim = NULL
  ){

    #### Check variable types
    # Convert characters to factors, if necessary.
    if(is.character(x)) x <- factor(x)
    # Convert factors to numbers, if necessary, so that limits can be calculated.
    if(is.factor(x)) x <- 1:length(levels(x))

    #### If the user has not defined limits, define limits.
    if(is.null(lim)){

      #### Option (a): Extract limits from axis positions (e.g., at) if provided.
      if(!is.null(at)){
        lim <- range(at)

      #### Option (b) Define initial limits based on the data
      } else {
        lim <- range(x, na.rm = TRUE)
        attributes(lim)$user <- FALSE
        # Check that both limits are not identical.
        # If so, adjust them: you cannot have a graph
        # ... with identical lower/upper limits
        if(length(unique(lim)) == 1){
          warning("Lower and upper limits for one of the inputted variables are the same. This is usually because all values of this variable are identical. Limits and pretty labels are being adjusted, but manually inputted limits may be necessary...\n")
          # Set lower limit to 0 or minus 1:
          if(lim[2] > 0){
            lim[1] <- 0
          } else{
            lim[1] <- lim[1] - 1
          }
        }
      }

    #### If the user has defined limits
    } else{

      #### Check that two limits are provided
      length_lim <- length(lim)
      if(length_lim != 2) stop(paste0(length_lim, " value(s) supplied as the limits for at least one axis; if supplied, two limits  are required."))
      #### Check that provided limits are sensible
      if(lim[1] >= lim[2]) { stop("Nonsensical user-specified axis limits: the lower limit for at least one axis is greater than or equal to the upper limit for the same axis. \n")}

      #### Define attributes
      attributes(lim)$user <- TRUE
    }

    #### Return limits
    return(lim)
  }


###################################
###################################
#### pretty_seq()

#' @title Define a pretty sequence given data, limits and pretty parameters
#' @description This function defines a pretty sequence given data, limits and parameters.
#' @param x An object.
#' @param lim A vector of length two which define limits, from define_lim_init().
#' @param pretty_args A named list of arguments passed to pretty_x().
#'
#' @return A list comprising 'at', a pretty sequence; and 'lim', limits, which may have been adjusted.
#'
#' @examples
#' pretty_seq(0:10)
#' pretty_seq(0:10, lim = c(0, 9))
#' pretty_seq(seq.Date(as.Date("2016-01-01"), as.Date("2016-01-12"), 1))
#' pretty_seq(seq.Date(as.Date("2016-01-01"), as.Date("2016-01-12"), 1), pretty_args = list(n = 3))
#'
#' @author Edward Lavender
#' @export
#'

pretty_seq <-
  function(x,
           lim = NULL,
           pretty_args = list(n = 5)){

    #### Check and/or define limits
    if(!is.null(lim)){
      # Check limit length
      if(length(lim) != 2) stop("lim must be a vector of length two.")
      # Check limit attributes
      if(is.null(attributes(lim)$user)) attributes(lim)$user <- TRUE
    } else{
      # Define limits
      lim <- define_lim_init(x = x, at = NULL, lim = NULL)
    }

    #### Define pretty sequence
    pretty_args$x <- lim
    if(is.null(pretty_args$n)) pretty_args$n <- 5
    if(is.factor(x)){
      pretty_args$by <- round(lim[2]/pretty_args$n)
      pretty_args$n <- NULL
    }
    at <- do.call(pretty_x, pretty_args)

    #### Clip pretty sequence within user-specified limits
    # If lim has been specified by the user...
    # the axes will stretch the full length of the limits but only have labels in the region of interest
    # This is dealt with at the plotting stage.
    # At this stage, we need to remove any pretty labels outside limits:
    at <- clip_within_range(at, lim)

    #### Adjust limits to ensure consistency between limits, axis positions and data
    # If (a) limits are not user specified and (b) we are not dealing with a factor
    if(!attributes(lim)$user & !is.factor(x)){
      # Calculate the interval between adjacent positions:
      interval <- diff_x(at[2], at[1])
      # Ensure min and max values of the data are within the axes:
      if(min(at) > min(x, na.rm = TRUE)) {
        at <- sort(c(min(at) - interval, at))
      }
      if(max(at) < max(x, na.rm = TRUE)) {
        at <- sort(c(at, max(at) + interval))
      }
      # Redefine axis limits:
      lim <- range(at); attributes(lim)$user <- FALSE
    }

    #### Define outputs
    out <- list(at = at, lim = lim)
    return(out)

  }


###################################
###################################
#### pretty_labels()

#' @title Define pretty labels
#' @description This function defines pretty axis labels from an object.
#' @param x An object, such as a numeric vector.
#' @param at A numeric vector of axis positions.
#' @details For factors, factor levels at positions specified by \code{at} are taken as labels. For numbers, \code{\link[plot.pretty]{add_lagging_point_zero}} and \code{\link[plot.pretty]{sci_notation}} are implemented as necessary to define pretty labels.
#' @return A vector of labels, of the same length as axis tick marks (\code{at}).
#' @author Edward Lavender
#' @keywords internal

pretty_labels <-
  function(x, at){
    if(is.factor(x)){
      lat <- length(at)
      llabels <- length(levels(x))
      labels <- rep(NA, lat)
      labels <- levels(x)[at]
    } else if(is_number(x)){
      # labels <- at
      labels <- sci_notation(at)
      if(is_number(labels)) labels <- add_lagging_point_zero(x = labels, n = NULL, ignore = TRUE)
    } else{
      labels <- NULL
    }
    return(labels)
  }


###################################
###################################
####  choose_foo_axis()

#' @title Choose an axis function
#' @description This function returns a function to add an axis to a plot, depending on the input.
#' @param at A sequence of positions at which the axis is drawn. Numbers or times (i.e., dates or POSIXct objects) are supported.
#' @return The function returns either \code{\link[graphics]{axis}}, \code{\link[graphics]{axis.Date}} or \code{\link[graphics]{axis.POSIXct}} depending on the input.
#' @author Edward Lavender
#' @keywords internal

choose_foo_axis <-
  function(at){
    if(is_number(at)){
      add_axis <- graphics::axis
    } else if(inherits(at, "POSIXct")){
      add_axis <- graphics::axis.POSIXct
    } else if(inherits(at, "Date")){
      add_axis <- graphics::axis.Date
    }
    return(add_axis)
  }


###################################
###################################
#### list_adjust()

#' @title Adjust lists to ensure correct mapply loops
#' @description This function adjusts lists to ensure mapply loops over inputted arguments correctly in \code{\link[plot.pretty]{pretty_axis}}.
#' @param l A list.
#' @param f A function used to adjust the list.
#' @param side Input to \code{side} argument of \code{\link[plot.pretty]{pretty_axis}}.
#' @return The function returns an (adjusted) list.
#' @author Edward Lavender
#' @keywords internal

list_adjust <- function(l, f = plotrix::listDepth, side){
  if(f(l) == 1){
    if(plotrix::listDepth(l) == 1){
      l <- lapply(side, function(a){ l })
    }
  }
  return(l)
}



#### End of code.
###################################
###################################
