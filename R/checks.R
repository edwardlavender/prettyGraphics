######################################
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
#' @examples
#' #### Example (1) Imagine we have a function in wich xlim and ylim cannot be supplied via ...
#' # Internally, within that function, we can implement check as follows:
#' pf <- function(...){
#'       check...(not_allowed = c("xlim", "ylim"),...)
#'       plot(1:10, 1:10, xlim = c(1, 10), ylim = c(1, 10),...)
#'       }
#' # This works:
#' pf(col = "red")
#' # This returns an error
#' \dontrun{
#' pf(col = "red", xlim = c(1, 15))
#' }
#'
#' @author Edward Lavender
#' @export


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
#' @examples
#'
#' #### Define an example function:
#' # The function returns 1 or 2, depending on the input to 'output'
#' return_1_or_2 <- function(output = 1){
#'   # Check the output, changing the output to the default if necessary
#'   output <- check_input(arg = "output", input = output, supp = 1:2, default = 1)
#'   # Return a value according to 'output'
#'   if(output == 1) return(1) else if(output == 2) return(2)
#' }
#'
#' #### Example (1): If a supported input to output is provided, everything works perfectly:
#' return_1_or_2(1); return_1_or_2(2)
#'
#' #### Example (2): # If an unsupported input to output is provided,
#' # ... the default output is used with a warning:
#' \dontrun{
#' return_1_or_2(3)
#' }
#'
#' @author Edward Lavender
#' @export
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


#### End of code.
######################################
######################################
