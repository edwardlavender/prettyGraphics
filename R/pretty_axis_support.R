###################################
###################################
#### factor extremes

#' @title Factor extremes
#' @description These functions return the minimum/maximum (first/last) levels of a factor.
#' @param x A factor.
#' @return A factor.
#'
#' @examples
#' x <- factor(LETTERS)
#' min_factor(x)
#' max_factor(x)
#' range_factor(x)
#' @author Edward Lavender
#' @name extreme_factor
NULL

#### min.factor()
#' @rdname extreme_factor
#' @export

min_factor <-
  function(x){
    stopifnot(inherits(x, "factor"))
    x <- factor(levels(x)[1], levels = x)
    return(x)
  }

#### max_factor()
#' @rdname extreme_factor
#' @export

max_factor <-
  function(x){
    stopifnot(inherits(x, "factor"))
    x <- factor(levels(x)[length(levels(x))], levels = x)
    return(x)
  }

#### range_factor()
#' @rdname extreme_factor
#' @export

range_factor <- function(x){
  stopifnot(inherits(x, "factor"))
  x_1 <- as.character(min_factor(x))
  x_2 <- as.character(max_factor(x))
  x   <- factor(c(x_1, x_2), levels = levels(x))
  return(x)
}


###################################
###################################
#### is_number()

#' @title Check if an object is a number
#' @description This function checks if an input is a number (i.e., an integer or numeric object).
#' @param x An object.
#' @param first A logical variable that defines whether or not to select \code{x[[1]]}. This avoids issues with some inputs (e.g., \code{scale(x)} is a matrix).
#' @return A logical value which defines whether or not \code{x} is a number.
#' @author Edward Lavender
#' @keywords internal

is_number <-
  function(x, first = FALSE){
    if (first) x <- x[[1]]
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

#' @title Define units within \code{\link[prettyGraphics]{pretty_axis}}
#' @description This function defines suitable default units (i.e., distances between sequential observations in a regular sequence) for a given input type (numbers or times).
#' @param iunits A number or character. If \code{NULL}, the function defines suitable units based on the object class (see \code{x} and Value). Otherwise, the function returns \code{iunits} unchanged.
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
    if(inherits(x1, "Date")){
      s <- seq.Date(x1, x2, by = units)
    } else if(inherits(x1, "POSIXct")){
      s <- seq.POSIXt(x1, x2, by = units)
    }
  }
  return(s)
}



###################################
###################################
#### pretty_x()

#' @title Create a regular pretty sequence
#' @description This function creates a regular pretty sequence from numbers or times.
#' @param An object.
#' @param x A vector of numbers or times.
#' @param ... Other arguments passed to \code{\link[base]{pretty}} or \code{\link[lubridate]{pretty_dates}}.
#' @return The function returns a series of pretty numbers or times.
#' @author Edward Lavender
#' @seealso \code{link[prettyGraphics]{pretty_seq}} is an extension of this function.
#' @keywords internal

pretty_x <- function(obj, x,...){

  #### Method for numbers
  if(is_number(obj)){
    s <- pretty(x,...)

  #### Method for times
  } else if(is_time(obj)){
    s <- lubridate::pretty_dates(x,...)
    # pretty_dates() converts objects to POSIXct so, we'll
    # convert back to a Date if necessary, so that axes are defined
    # on the same scale as the data
    if(inherits(obj, "Date")) s <- as.Date(s)

  #### Method for factors
  } else if(is.factor(obj)){
    s <- seq(min(as.integer(x), na.rm = TRUE), max(as.integer(x), na.rm = TRUE),...)
  }

  #### Return sequence
  return(s)
}


###################################
###################################
#### seq_extend()

#' @title Extend a regular sequence within limits
#' @description This function extends a regular sequence in both directions towards user-defined limits.
#' @param x A sequence.
#' @param lim A vector of two which specify the lower and upper limits.
#' @return The function returns a sequence.
#' @examples
#' # extend_seq(2:4, c(0, 10))
#' # extend_seq(2:4, c(0, 4))
#' # x <- seq.Date(as.Date("2016-01-01"), as.Date("2016-01-10"), 1)
#' # extend_seq(x, as.Date(c("2015-12-10", "2016-01-20")))
#' @author Edward Lavender
#' @keywords internal
#'
seq_extend <- function(x, lim){
  stopifnot(length(lim) == 2)
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  stopifnot(min_x >= lim[1] & max_x <= lim[2])
  stopifnot(!is.null(x[1]) & !is.null(x[2]))
  delta <- x[2] - x[1]
  left <- seq(lim[1], min_x, by = delta)
  if(length(left) > 1){
    left <- left[1:(length(left) - 1)]
    x <- c(left, x)
  }
  right <- seq(max_x, lim[2], by = delta)
  if(length(right) > 1){
    right <- right[2:length(right)]
    x <- c(x, right)
  }
  return(x)
}


###################################
###################################
#### define_lim_init()

#' @title Define initial limits from a sequence of numbers.
#' @description. This function defines initial limits for a sequence of numbers.
#' @param x A vector which contains user-supplied data.
#' @param at (optional) A vector which contains user-supplied axis positions.
#' @param lim (optional) A vector of length two which contains user-supplied limits.
#' @details If the user has supplied limits, this function checks limits to ensure that they are sensible, and flags them with a 'user' attribute (so that user-defined limits are recognisable and remain fixed). If the user has not supplied limits, limits are extracted from axis tick mark positions, if specified, or otherwise from the data provided.
#' @return This function returns a vector of two numbers representing initial limits.
#' @author Edward Lavender
#' @keywords internal

define_lim_init <-
  function(x,
           at = NULL,
           lim = NULL
  ){

    #### Return limits unchanged, if provided in full
    if(length(lim) == 2 & length(which(!is.na(lim))) == 2 & length(attributes(lim)$user) == 2){
      return(lim)
    }

    #### Check variable types
    # Convert characters to factors, if necessary.
    if(is.character(x)) x <- factor(x)
    # Check that numerical limits have not been provided for a factor
    if(is.factor(x)){
      if(!is.null(lim)){
        warning("Limit(s) supplied for factors are ignored. In pretty_axis(), use control_factor_lim to adjust internally defined limits.")
        lim <- NULL
      }
    }
    # Convert factors to numbers, if necessary, so that limits can be calculated.
    if(is.factor(x)) x <- 1:length(levels(x))

    #### Limit pre-processing prior to limit definition
    # If limits have not been provided, set both to NA
    if(is.null(lim)) {
      # Define limits from x first, to preserve the class of x.
      lim <- rep(x[1], 2)
      lim[1] <- NA; lim[2] <- NA;
    }
    # If only one limit has been provided, assume the first limit and set the second to NA
    if(length(lim) == 1) lim <- c(lim, NA)

    #### Loop over each limit, define value (if necessary) and attributes
    lim_ls <-
      lapply(1:2, function(i){

        #### Extract value of limit and function to define limits, if necessary
        ilim <- lim[i]
        # Lower limits are given from the minimum of axis positions/data;
        # Uper limits are given from the maximum of axis positions/data;
        define_lim <- list(min, max)[[i]]

        #### Limits do not need to be defined
        if(!is.na(ilim)){
          attributes(ilim)$user <- TRUE

          #### Limits need to be defined
        } else{
          ## Option (a): Extract limits from axis positions (e.g., at) if provided.
          if(!is.null(at)){
            ilim <- define_lim(at)
            attributes(ilim)$user <- TRUE
            ## Option (b) Define initial limits based on the data
          } else {
            # Define limits
            ilim <- define_lim(x, na.rm = TRUE)
            attributes(ilim)$user <- FALSE
          }
        }
        return(ilim)
      })
    # Use do.call() to unlist list to preserve attributes
    lim <- do.call("c", lim_ls)
    attributes(lim)$user <- sapply(lim_ls, function(i) attributes(i)$user)

    #### Final checks
    # Check that both limits are not identical.
    # If so, adjust them: you cannot have a graph with identical lower/upper limits.
    if(length(unique(lim)) == 1){
      message("Lower and upper limits for an inputted variable are identical. This is usually because all values of this variable are identical. Adjusted limits (+/- 25 % or +/- 0.25 or +/- 60 s) implemented.\n")
      # Adjust limits
      if(is_number(lim)){
        if(lim[1] != 0){
          if(lim[1] > 0){
            lim[1] <- lim[1] - (0.25*lim[1])
            lim[2] <- lim[2] + (0.25*lim[2])
          } else {
            lim[1] <- lim[1] + (0.25*lim[1])
            lim[2] <- lim[2] - (0.25*lim[2])
          }
        } else{
          lim[1] <- lim[1] - 0.25
          lim[2] <- lim[2] + 0.25
        }
      } else {
        lim[1] <- lim[1] - 60
        lim[2] <- lim[2] + 60
      }
    }
    # Check that limits are sensible
    if(lim[1] >= lim[2]) {
      stop("Nonsensical user-specified axis limits: the lower limit for at least one axis is greater than or equal to the upper limit for the same axis. \n")
    }

    #### Return limits
    return(lim)
  }


###################################
###################################
#### pretty_seq()

#' @title Define a pretty sequence given data, limits and pretty parameters
#' @description This function defines a pretty sequence given data, limits and parameters.
#' @param x An object representing some data to be plotted (e.g., x values).
#' @param lim (optional) A vector of length two which define the limits within which the sequence should lie.
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

    #### Check inputs
    x <- check_input_class(arg = "x", input = x,
                           if_class = "character", to_class = "factor",
                           type = "warning", coerce_input = factor)

    #### Define initial limits
    # If these have been provided, they are unchanged.
    lim <- define_lim_init(x = x, lim = lim, at = NULL)

    #### Define pretty sequence
    # Save object
    pretty_args$obj <- x
    # Define 'x' argument of pretty args using limits unless factor with only one level
    # ... then simply use supplied x
    if(is.factor(x) & length(levels(x)) == 1) pretty_args$x <- x else pretty_args$x <- lim
    if(is.null(pretty_args$n)) pretty_args$n <- 5
    if(is.factor(x)){
      if(pretty_args$n > lim[2]){
        message("pretty$n greater than the number of factor levels; resetting n to be the total number of factor levels.")
        pretty_args$by <- 1
      } else pretty_args$by <- ceiling(lim[2]/pretty_args$n)
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
    if(!is.factor(x)){
      interval <- diff_x(at[2], at[1])
      for(i in 1:2){
        if(!attributes(lim)$user[i]){
          # Ensure min and max values of the data are within the axes:
          if(i == 1){
            if(min(at) > min(x, na.rm = TRUE)) {
              at <- sort(c(min(at) - interval, at))
            }
            lim[1] <- min(at)
          } else if(i == 2){
            if(max(at) < max(x, na.rm = TRUE)) {
              at <- sort(c(at, max(at) + interval))
            }
            lim[2] <- max(at)
          }
        }
      }
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
#' @param n (optional) An integer which defines the number of decimal places for numeric axes. This is passed to \code{\link[prettyGraphics]{add_lagging_point_zero}}. If \code{NULL}, \code{n} is defined internally.
#' @param pi_notation_args A named list of arguments passed to \code{\link[prettyGraphics]{pi_notation}} (excluding \code{x}).
#' @param sci_notation_args A named list of arguments passed to \code{\link[prettyGraphics]{sci_notation}} (excluding \code{x}).
#' @details For factors, factor levels at positions specified by \code{at} are taken as labels. For numbers, \code{\link[prettyGraphics]{add_lagging_point_zero}} and \code{\link[prettyGraphics]{sci_notation}} are implemented as necessary to define pretty labels.
#' @return A vector of labels, of the same length as axis tick marks (\code{at}).
#' @author Edward Lavender
#' @keywords internal

pretty_labels <-
  function(x, at, n = NULL, pi_notation_args = NULL, sci_notation_args = list()){
    if(is.factor(x)){
      lat <- length(at)
      llabels <- length(levels(x))
      labels <- rep(NA, lat)
      labels <- levels(x)[at]
    } else if(is_number(x)){
      labels <- at
      if(!is.null(pi_notation_args)){
        pi_notation_args$x <- at
        labels <- do.call(pi_notation, pi_notation_args)
      }
      if(is_number(labels) & !is.null(sci_notation_args)){
        sci_notation_args$x <- at
        labels <- do.call(sci_notation, sci_notation_args)
      }
      if(is_number(labels)) {
        labels <- add_lagging_point_zero(x = labels, n = n, ignore = TRUE)
      }
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
#### empty_list_to_list_null

#' @title Replace \code{list()} with \code{list(NULL)}
#' @description This function replaces an empty list with \code{list(NULL)}.
#' @param list_name The name of the list.
#' @param l A list.
#' @return A list, as inputted, if the inputted list is not empty; or list(NULL) if the input is an empty list.
#' @author Edward Lavender
#' @keywords internal

empty_list_to_list_null <-
  function(list_name, l){
    check_input_class(arg = list_name, input = l, if_class = NULL, to_class = "list", type = "stop")
    if(length(l) == 0) return(list(NULL)) else return(l)
  }


###################################
###################################
#### list_adjust()

#' @title Adjust lists to ensure correct mapply loops
#' @description This function adjusts lists to ensure mapply loops over inputted arguments correctly in \code{\link[prettyGraphics]{pretty_axis}}.
#' @param l A list.
#' @param f A function used to adjust the list.
#' @param side Input to \code{side} argument of \code{\link[prettyGraphics]{pretty_axis}}.
#' @return The function returns an (adjusted) list.
#' @author Edward Lavender
#' @keywords internal

list_adjust <- function(l, f = list_depth, side){
  if(f(l) == 1){
    if(list_depth(l) == 1){
      l <- lapply(side, function(a){ l })
    }
  }
  return(l)
}



#### End of code.
###################################
###################################
