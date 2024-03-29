#' @title Add lagging .0(s) to a number
#' @description This function adds lagging .0(s) to a number. This is useful for tidy graph labels. This is implemented internally in \code{\link[prettyGraphics]{pretty_axis}}.
#'
#' @param x A numeric or character vector which contains some elements to which lagging .0(s) should be added.
#' @param n A number which defines the desired number of decimal places. Any element in \code{x} with fewer than \code{n} decimal places will have the appropriate number of decimal places added. If \code{n} is less than the number of decimal places for any element in \code{x}, the function will return an error. In this case, round all numbers to the same precision first, before implementing \code{\link[prettyGraphics]{add_lagging_point_zero}}. If \code{n = NULL}, the function defines \code{n} internally to be the maximum number of decimal places in \code{x}.
#' @param ignore A logical input which specifies whether or not, under the condition that no number has any decimal places, the \code{n} argument should be ignored (i.e. if \code{TRUE}, the input vector, \code{x}, is simply returned without any adjustments).
#'
#' @details This function is not designed to work with scientific notation. Digits for scientific notation can be controlled via \code{\link[prettyGraphics]{sci_notation}}.
#'
#' @return A vector, as inputted, but in which any elements with fewer than \code{n} decimal places have had ".0"(s) added.
#'
#' @examples
#' #### Example (1): Bring all numbers up to the same number of decimal places
#' # Use maximum number of decimal places of any one number (i.e., default n specification):
#' add_lagging_point_zero(c(0.01, 0.002))
#' # Specify desired number of decimal places
#' add_lagging_point_zero(c(0.01, 0.002), n = 4)
#'
#' #### Example (2): More examples
#' add_lagging_point_zero(seq(0, 1000, by = 100), 1)
#' add_lagging_point_zero(c(50.123, 1000, 150, 2000), 3)
#' add_lagging_point_zero(c(50.0, 1000, 150, 2000), 1)
#'
#' #### Example (3): the ignore argument returns an unchanged character vector if no numbers
#' # ... in the vector have decimal places
#' add_lagging_point_zero(seq(0.1, 1000, by = 100), 2, ignore = TRUE) # decimal places added
#' add_lagging_point_zero(seq(0, 1000, by = 100), 2, ignore = TRUE)   # decimal places not added
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
#' #### Example (4): add_lagging_point_zero() is not designed to work with scientific notation
#' # ... Use sci_notation() instead:
#' add_lagging_point_zero(1e9, n = 2)
#' sci_notation(1e9, digits = 2)
#'
#' @author Edward Lavender
#' @export
#'

add_lagging_point_zero <-
  function(x, n = NULL, ignore = FALSE){
    index_na <- which(is.na(x))
    xc <- format(x, scientific = FALSE, trim = TRUE)
    dp <- stringr::str_split_fixed(xc, "[.]", 2)[, 2]
    if(all(nchar(dp) == 0) && ignore) return(x)
    x <- xc
    if(is.null(n)) n <- max(nchar(dp))
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
    x[index_na] <- NA
    return(x)
    }
