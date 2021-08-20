######################################
######################################
#### utils

######################################
######################################
#### pipe

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


######################################
######################################
#### list_depth()

#' @title Find the maximum depth of a list
#' @description Descend a list and find the maximum number of levels in a list.
#' @details A possibly nested list of lists is descended to determine the maximum number of levels.
#' @return The maximum number of levels in the list.
#' @source This function and the documentation are derived from \code{\link[plotrix]{listDepth}}. The function is defined separately in \code{\link[prettyGraphics]{prettyGraphics}} to reduce reliance on non-default packages.
#' @keywords internal
#'

list_depth <- function(x) {
  if(is.list(x)) {
    maxdepth<-1
    for(lindex in seq_along(x)) {
      newdepth<-list_depth(x[[lindex]])+1
      if(newdepth > maxdepth) maxdepth<-newdepth
    }
  }
  else maxdepth<-0
  return(maxdepth)
}

######################################
######################################
#### plyr helpers


######################################
#### round_any()

#' @title Round to multiple of any number
#' @description Round to multiple of any number.
#' @param x numeric or date-time (POSIXct) vector to round
#' @param accuracy number to round to; for POSIXct objects, a number of seconds
#' @param f rounding function: floor, ceiling or round
#' @source This function and the documentation are taken from the `plyr' package. The function is defined separately in \code{\link[prettyGraphics]{prettyGraphics}} to reduce reliance on non-default packages.
#' @keywords internal
#'

round_any <- function(x, accuracy, f = round){f(x/accuracy) * accuracy}


######################################
#### compact()

#' @title Compact a list
#' @description Remove all \code{NULL} entries from a list.
#' @param l A list.
#' @source This function is derived from the \code{plyr::compact()} function. The function is defined separately in \code{\link[prettyGraphics]{prettyGraphics}} to reduce reliance on non-default packages.
#' @keywords internal

compact <- function(l) l[which(!sapply(l, is.null))]


######################################
######################################
#### Checks


######################################
#### check_names()

#' @title Check the names of an object contain required names
#' @description This function checks whether required names are contained within an object. If the object does not contain any/all required names (the precise criteria is controlled by the user), the function returns a helpful error message.
#' @param arg A character string which defines the argument of the parent function.
#' @param input An object for which the names need to be checked.
#' @param req A character vector of required names.
#' @param extract_names A function which is used to extract names from \code{input}, such as \code{\link[base]{names}} or \code{\link[base]{colnames}}.
#' @param type A function which defines the failure criteria. For example, if \code{type = all}, the function will return an error unless all the names in \code{req} are contained within \code{input}. This is the default. If \code{type = any}, the function will return an error only if none of the names in \code{req} are contained within \code{input}.
#' @return If the input fails the check, the function returns a helpful error message. Otherwise, nothing is returned.
#' @author Edward Lavender
#' @keywords internal
#'

check_names <- function(arg = deparse(substitute(input)), input, req, extract_names = names, type = any){
  input_names <- extract_names(input)
  if(!type(req %in% input_names)){
    req_names_missing <- req[which(!(req %in% input_names))]
    msg <- paste0("Argument ", arg, " does not contain ", deparse(substitute(type)), " required names. The following name(s) are missing:",
                  paste0("'", req_names_missing, collapse = ", "),
                  "'.")
    stop(msg)
  }
}

######################################
#### check...()

#' @title Check that arguments supplied via ... are allowed
#' @description This function checks that arguments supplied via ... are allowed. This function was written to support other functions, specifically via the return of a helpful error message if arguments that cannot be supplied via ... have been supplied. The function is not intended for general use.
#'
#' @param not_allowed A character vector of the names of function arguments that are not allowed.
#' @param ... Other arguments
#'
#' @return The function checks other arguments supplied via ...; if these contain an argument that is not allowed, the function returns an error. Otherwise, nothing is returned.
#'
#' @author Edward Lavender
#' @keywords internal


check... <- function(not_allowed,...){
  l <- list(...)
  if(any(names(l) %in% not_allowed)){
    trouble <- names(l)[names(l) %in% not_allowed]
    msg <- paste0("Additional arguments (", paste(trouble, collapse = ", "),
                  ") have been passed to the function via ... which are implemented internally or need to be supplied via other function arguments. Implement these options via appropriate function arguments, if possible, or do not supply them.")
    stop(msg)
  }
}


######################################
#### check_input_value()

#' @title Check the input to a parent function argument
#' @description Within a function, this function checks the input to an argument of that function. If the input is supported, the function simply returns this value. If the input is not supported, the function returns a warning and the default value. This function is designed to be implemented internally within functions and not intended for general use.
#'
#' @param arg A character string which defines the argument of the parent function.
#' @param input The input to an argument of a parent function.
#' @param supp A vector of supported input values for the argument in the parent function.
#' @param default The default input value for the parent function.
#'
#' @return The function returns \code{input} or \code{default} (the latter with a warning) depending on whether or not \code{input} is within \code{supp} (i.e., whether or not the input to the argument of a parent function is supported).
#'
#' @author Edward Lavender
#' @keywords internal
#'

check_input_value <- function(arg, input, supp, default = supp[1]){
  # If the input is not in a vector of supported arguments...
  if(!(input %in% supp)){
    # Provide a warning and revert to the default
    warning(paste0("Input to argument ", arg, " (", input, ") is not supported; defaulting to ", arg, " = ", default, ".\n"))
    input <- default
  }
  # Return input
  return(input)
}


###################################
#### check_input_class()

#' @title Check the class of an function input to a parent function
#' @description This function checks that the class of an input to a parent function is appropriate. If not, the function either produces a helpful error message or returns a warning.
#' @param arg A character string which defines the argument of the parent function.
#' @param input The input to an argument of a parent function.
#' @param if_class (optional) A character vector of classes of object. If supplied, the function will only proceed to check the class of the object if the \code{class(input)} is one of \code{if_class}. This is useful if \code{check_input_class()} is implemented in a loop.
#' @param to_class The required class of the input.
#' @param type A character which specifies whether to return an error (\code{"stop"}) or a warning ("warning").
#' @param coerce_input A function used to coerce \code{input} to the correct object type, if \code{type = "warning"}.
#' @return The function checks the class of the input. If the class is not the same as required by the parent function (i.e., as specified by \code{class}), the function returns a helpful error message, or a warning and an object whose class has been coerced to the correct class.
#'
#' @author Edward Lavender
#' @keywords internal
#'

check_input_class <-
  function(arg, input, if_class = NULL, to_class, type = "stop", coerce_input){

    #### Define whether or not to proceed:
    # Only proceed if if_class is NULL or, if supplied, then only proceed if the class of the object
    # ... is of type(s) in if_class
    proceed <- FALSE
    if(is.null(if_class)){
      proceed <- TRUE
    } else{
      if(inherits(input, if_class)) proceed <- TRUE
    }

    #### Check the class, if required
    if(proceed){
      # If the object is not of the necessary class
      if(!inherits(input, to_class)){
        # Either stop...
        if(type == "stop"){
          msg <- paste0("Argument '", arg, "' must be of class '", to_class, "', not class(es): '", paste(class(input), collapse = "', '"), "'.")
          stop(msg)
          # Or print a warning and use coerce_input() to convert the object to the desired class.
        } else if(type == "warning"){
          msg <- paste0("Argument '", arg, "' coerced to class '", to_class, "' from class(es): '", paste(class(input), collapse = "', '"), "'.")
          warning(msg)
          input <- coerce_input(input)
        }
      }
    }

    #### If we've passed all checks, return the input (possibly coerced to a new class)
    return(input)
  }


######################################
#### check_tz()

#' @title Check the timezone of an object and force UTC if absent
#' @description This function checks the time zone of an inputted  object. If the object is of class Date or POSIXct and a time zone is absent, then "UTC" is forced. Otherwise, the object is returned unchanged.
#' @param arg (optional) A character string which defines the argument of the parent function.
#' @param x An object.
#' @return An object as inputted in which, if the object is of class Date or POSIXct and a time zone is absent, time zone "UTC" is forced.
#' @author Edward Lavender
#' @keywords internal

check_tz <-
  function(arg = NULL, x){
    if(inherits(x, "Date") | inherits(x, "POSIXct")){
      if(lubridate::tz(x) == ""){
        if(is.null(arg)){
          msg <- "time zone currently ''; tz forced to UTC."
        } else{
          msg <- paste0("Argument '", arg, "' time zone currently ''; tz forced to UTC.")
        }
        warning(msg)
        lubridate::tz(x) <- "UTC"
      }
    }
    return(x)
  }


###################################
#### check_named_list()

#' @title Check that a list is named
#' @description This function checks that the top level of a list is named (ignoring empty lists if requested). If the list is not named, the function returns a helpful error message. Otherwise, the list is returned unchanged. This is particularly useful within functions that use \code{\link[base]{do.call}} to evaluate lists of arguments.
#' @param arg (optional) A character string which defines the argument of a parent function.
#' @param l A list.
#' @param ignore_empty A logical input which defines whether or not to ignore empty lists.
#' @return The function returns a helpful error message for unnamed lists (ignoring empty lists if requested) or the inputted list unchanged.
#'
#' @author Edward Lavender
#' @keywords internal

check_named_list <- function(arg = NULL, l, ignore_empty = TRUE){
  if(list_depth(l) > 1){
    warning("Input list of check_named_list() is of depth > 1; only the top level is checked.")
  }
  list_is_empty <- (length(l) == 0)
  if(!list_is_empty | !ignore_empty){
    if(is.null(names(l)) | any(names(l) %in% "")){
      if(is.null(arg)){
        msg <- "Argument must be a named list."
      } else {
        msg <- paste0("Argument '", arg, "' must be a named list.")
      }
      stop(msg)
    }
  }
  return(l)
}

#####################################
#### clip_within_range()

#' @title Clip a vector to lie within a range
#' @description This function clips a vector to lie within a range.
#' @param x A vector, to be clipped.
#' @param range A vector of two numbers, used to clip \code{x}.
#' @return A vector, as inputted, solely comprising values within the range specified by \code{range}, inclusive.
#' @author Edward Lavender
#' @keywords internal

clip_within_range <-
  function(x, range){
    stopifnot(length(range) == 2)
    pos2rem <- which(x < range[1])
    if(length(pos2rem) > 0) x <- x[-c(pos2rem)]
    pos2rem <- which(x > range[2])
    if(length(pos2rem) > 0) x <- x[-c(pos2rem)]
    return(x)
  }


######################################
#### list_merge()

#' @title Merge lists accounting for empty lists
#' @description This function is a wrapper for \code{\link[rlist]{list.merge}}. The difference is that this function first screens out any empty lists, which cause errors for \code{\link[rlist]{list.merge}}. If there is only one non-empty list, this is returned. Otherwise, \code{\link[rlist]{list.merge}} is used to merge lists in an iterative process. For large lists, this approach will be slower than calling \code{\link[rlist]{list.merge}} directly if there are no empty lists. Both \code{\link[rlist]{list.merge}} and \code{list_merge()} require named lists.
#'
#' @param ... named lists
#' @keywords internal

list_merge <- function(...){
  # Define overall list
  lists <- list(...)
  # Identify empty lists
  pos_empty <- which(sapply(lists, function(x) length(x) == 0))
  # Remove any empty lists
  if(length(pos_empty) > 0){
    lists[pos_empty] <- NULL
  }
  # If there is only one list left, simply return that list
  if(length(lists) == 1){
    return(unlist(lists, recursive = FALSE))

    # Otherwise, use rlist::list.merge() to join lists
  } else{
    # Define the first list
    l <- lists[[1]]
    # Iteractively add to this list
    for(i in 2:length(lists)){
      l <- rlist::list.merge(l, lists[[i]])
    }
    return(l)
  }
}


######################################
#### check_for_depreciated()

#' @title Check for depreciated arguments passed via \code{...}
#' @description This function checks for depreciated arguments passed via \code{...} to a parent function and returns a warning if any depreciated arguments have been used.
#' @param depreciated A character vector that defines the names of depreciated arguments to a parent function.
#' @param ... Additional arguments passed to a parent function.
#' @author Edward Lavender
#' @keywords internal

check_depreciated <- function(depreciated,...){
  dots <- list(...)
  if(any(names(dots) %in% depreciated)) {
    deprec <- names(dots)[names(dots) %in% depreciated]
    warn <- paste0("The following argument(s) are depreciated: ", paste0("'", deprec, collapse = "', "),"'.")
    warning(warn, call. = FALSE, immediate. = TRUE)
  } else return(invisible())
}


#### End of code.
######################################
######################################
