######################################
######################################
#### check functions
# Source: utils.add: https://github.com/edwardlavender/utils.add
# 19/06/2020

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
#### check_input()

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

check_input <- function(arg, input, supp, default = supp[1]){
  # If the input is not in a vector of supported arguments...
  if(!(input %in% supp)){
    # Provide a warning and revert to the default
    warning(paste0("Input to argument ", arg, " (", input, ") is not supported; defaulting to ", arg, " = ", default, ".\n"))
    input <- default
  }
  # Return input
  return(input)
}


######################################
######################################
#### list_merge() from utils.add
# Source: https://github.com/edwardlavender/utils.add
# 18/06/2020

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


#### End of code.
######################################
######################################
