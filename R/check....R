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
#' 

######################################
######################################
#### check...()


check... <- function(not_allowed,...){
  l <- list(...)
  if(any(names(l) %in% not_allowed)){
    trouble <- names(l)[names(l) %in% not_allowed]
    msg <- paste0("Additional arguments (", paste(trouble, collapse = ", "), 
                  ") have been passed to the function via ... which are implemented internally or need to be supplied via other function arguments. Implement these options via appropriate function arguments, if possible, or do not supply them.")
    stop(msg)
  }
}


#### End of code. 
######################################
######################################