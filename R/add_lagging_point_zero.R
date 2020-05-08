#' @title Add lagging .0(s) to a number
#' @description This function adds lagging .0(s) to a number. This is useful for tidy graph labels (e.g. with scientific notation).
#'
#' @param x A numeric or character vector which contains some elements to which lagging .0(s) should be added.
#' @param n A number which defines the desired number of decimal places. Any element in \code{x} with fewer than \code{n} decimal places will have the appropriate number of decimal places added. If \code{n} is less than the number of decimal places for any element in \code{x}, the function will return an error. In this case, round all numbers to the same precision first, before implementing \code{add_lagging_point_zero}.
#' @param ignore A logical input which specifies whether or not, under the condition that no number has any decimal places, the \code{n} argument should be ignored (i.e. if \code{TRUE}, the character vector, \code{x}, is simply returned without any adjustments).
#'
#' @return A character vector, as inputted, but in which any elements with fewer than \code{n} decimal places have had ".0"(s) added.
#'
#' @examples
#'
#' #### Example (1): Bring all numbers up to the same number of decimal places
#' add_lagging_point_zero(c(0.01, 0.002), n = 4)
#'
#' #### Example (2): More examples
#' add_lagging_point_zero(seq(0, 1000, by = 100), 1)
#' add_lagging_point_zero(c(50.123, 1000, 150, 2000), 3)
#' add_lagging_point_zero(c(50.0, 1000, 150, 2000), 1)
#'
#' #### Example (3): the ignore argument returns an unchanged character vector if no numbers
#' # ... in the vector have decimal places
#' add_lagging_point_zero(seq(0.1, 1000, by = 100), 1, ignore = TRUE) # decimal places added
#' add_lagging_point_zero(seq(0, 1000, by = 100), 1, ignore = TRUE)   # decimal places not added
#'
#' #### Example (3): Using add_lagging_point_zero() for prettier axis labels
#' # Numeric vector input
#' # Define some data/axis positions
#' at <- seq(0, 1, by = 0.2) * 10^5
#' # Plot graph
#' graphics::plot(at, at, axes = FALSE)
#'
#' # Labels before add_lagging_point_zero():
#' labels1 <- at/10^5; labels1
#' as.character(labels1) # the .0 disappears from 0 and 1 when coerced to a character (see graph)
#'
#' # Labels after add_lagging_point_zero():
#' labels2 <- add_lagging_point_zero(labels1, n = 1); labels2
#' as.character(labels2)
#'
#' # Labels before add_lagging_point_zero() are varying numbers of decimal places:
#' axis(side = 1, at, labels = labels1)
#'
#' # Labels after before add_lagging_point_zero() are tidier:
#' axis(side = 2, at, labels = labels2)
#'
#' @author Edward Lavender
#' @export
#'

################################################
################################################
#### add_lagging_point_zero

add_lagging_point_zero <-
  function(x, n, ignore = FALSE){
    x <- as.character(x)
    dp <- stringr::str_split_fixed(x, "[.]", 2)[, 2]
    if(all(nchar(dp) == 0) & ignore) return(x)
    diff <- n - nchar(dp)
    if(any(diff < 0)) stop("n is too small; some numbers have more decimal places than n.")
    pwd <- function(x, diff, n){
      if(diff == n){
        paste0(x, ".", paste0(rep(0, diff), collapse = ""))
      } else {
        paste0(x, paste0(rep(0, diff), collapse = ""))
      }
    }
    pwd <- Vectorize(pwd, vectorize.args = c("x", "diff"))
    x <- pwd(x, diff, n)
    x <- as.character(x)
    return(x)
  }

#### End of function.
################################################
################################################
