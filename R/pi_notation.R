######################################
######################################
#### pi_notation()

#' @title \eqn{\pi} notation
#' @description This function re-expresses a vector of numbers in terms of \eqn{\pi}, returning an \code{\link[base]{expression}} object that can be added to a plot.
#' @param x A numeric vector.
#' @param as_fraction A logical value that defines whether or not to express sequential multipliers of \eqn{\pi} as fractions (\code{TRUE}) or decimals (\code{FALSE}).
#' @param as_bar If \code{as_fraction = TRUE}, \code{as_bar} is a logical value that defines whether or not to style fractions with a horizontal bar (i.e., \eqn{\frac{a}{b}}) or a slash (i.e., \eqn{a/b}).
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{add_lagging_point_zero}} if \code{as_fraction = FALSE}.
#'
#' @details The function is designed to be used on a regular sequence of numbers (e.g., representing the tick marks on a plot) within \code{\link[prettyGraphics]{pretty_plot}}.
#' @return The function returns an \code{\link[base]{expression}} object.
#'
#' @examples
#' #### Example (1): Implement the function in isolation
#' x <- seq(0, 8, by = 0.25*pi)/pi
#' pi_notation(x)
#' pi_notation(x, as_bar = FALSE)
#' pi_notation(x, as_fraction = FALSE)
#'
#' #### Example (2): Plotting with fractions
#' ## Stacked fraction
#' xlabels <- pi_notation(x, as_fraction = TRUE)
#' plot(x, rep(0, length(x)), axes = FALSE)
#' axis(side = 1, at = x*pi, labels = xlabels, col = "red")
#' ## Spread fraction
#' x_axis <- pi_notation(x, as_fraction = TRUE, as_bar = FALSE)
#' plot(x, rep(0, length(x)), axes = FALSE)
#' axis(side = 1, at = x*pi, labels = xlabels, col = "red")
#'
#' #### Example (3): Plotting with decimals
#' x_axis <- pi_notation(1:3, as_fraction = FALSE)
#' plot(x, rep(0, length(x)), axes = FALSE)
#' axis(side = 1, at = x*pi, labels = xlabels, col = "red")
#'
#' #### Example (4): pi_notation() is implemented automatically in prettyGraphics
#' # ... via pretty_axis(), pretty_plot() etc.
#'
#' ## Define example time series of lunar phase in radians
#' dat <- data.frame(date = seq.Date(as.Date("2016-01-01"), as.Date("2016-02-01"), by = 1))
#' dat$lunar <- lunar::lunar.phase(dat$date)
#'
#' ## Visualise lunar phase time series, with pi_notation
#' pretty_plot(dat$date, dat$lunar,
#'             pretty_axis_args = list(pi_notation = list(NULL, list()))
#'             )
#' add_moons(side = 2, position = min(dat$date), radius = 0.5)
#'
#' ## Customise pi_notation
#' # Use as_bar = FALSE
#' pretty_plot(dat$date, dat$lunar,
#'             pretty_axis_args = list(pi_notation = list(NULL, list(as_bar = FALSE)))
#'             )
#' add_moons(side = 2, position = min(dat$date), radius = 0.5)
#' # Use decimals
#' pretty_plot(dat$date, dat$lunar,
#'             pretty_axis_args = list(pi_notation = list(NULL, list(as_fraction = FALSE)))
#'             )
#' add_moons(side = 2, position = min(dat$date), radius = 0.5)
#'
#' ## Use pi_notation within specified axis limits
#' pretty_plot(dat$date, dat$lunar,
#'             ylim = c(0, 2*pi),
#'             pretty_axis_args = list(pi_notation = list(NULL, list()))
#'             )
#' add_moons(side = 2, position = min(dat$date), radius = 0.5)
#'
#' @seealso \code{\link[prettyGraphics]{pretty_axis}} (and subsidiary plotting functions, such as \code{\link[prettyGraphics]{pretty_plot}}) can implement this function internally.
#' @author Edward Lavender
#' @export

pi_notation <- function(x,
                        as_fraction = TRUE,
                        as_bar = TRUE,...){

  #### Set up a list to store labels
  lab <- list()

  #### Fractional method
  if(as_fraction) {

    ## Define fractions
    x_seq <- x
    x_seq <- fractions(x_seq)
    # Define matrix for numerator ([, 1]) and denominator ([, 2])
    x_seq_c <- attr(x_seq, "fracs")
    x_seq_cmat <- stringr::str_split_fixed(x_seq_c, "/", n = 2)

    ## Make a list of fractional labels
    for(i in seq_along(x_seq)){
      ## If the number has been re-expressed as a fraction...
      if(x_seq_cmat[i, 2] != ""){
        ## Define label with bar fraction method
        if(as_bar) {
          lab[[i]] <- bquote(frac(.(x_seq_cmat[i, 1]), .(x_seq_cmat[i, 2])) * pi)
        } else {
          ## Define label with slash fraction method
          lab[[i]] <- bquote(.(x_seq_cmat[i, 1]) * "/" * .(x_seq_cmat[i, 2]) * pi)
        }
        ## Else, if the number isn't a fraction, simply define the label
      } else {
        lab[[i]] <- bquote(.(x_seq_cmat[i, 1]) * pi)
      }
    }

    #### Decimal method
  } else {
    x <- add_lagging_point_zero(x,...)
    for(i in seq_along(x)){
      lab[[i]] <- bquote(.(x[i]) * pi)
    }
  }

  #### Define outputs (tick marks and associated labels)
  lab <- do.call(expression, lab)
  return(lab)

}


######################################
######################################
#### MASS::fractions()

# file MASS/R/fractions.R
# copyright (C) 1994-2005 W. N. Venables and B. D. Ripley
# source: https://rdrr.io/cran/MASS/src/R/fractions.R

#' @title Local implementation of the \code{MASS::fractions()} function
#' @description Find rational approximations to the components of a real numeric object using a standard continued fraction method.
#' @param x Any object of mode numeric.
#' @param cycles The maximum number of steps to be used in the continued fraction approximation process.
#' @param max.denominator An early termination criterion.
#' @param ... Arguments passed to or from other methods.
#' @source This function and the documentation is taken directly from the MASS package (see https://rdrr.io/cran/MASS/src/R/fractions.R). It is defined locally in \code{\link[prettyGraphics]{prettyGraphics}} solely to reduce reliance on non-default packages.
#' @keywords internal

fractions <- function(x, cycles = 10, max.denominator = 2000, ...)
{
  .rat <- function(x, cycles = 10, max.denominator = 2000)
  {
    a0 <- rep(0, length(x))
    A <- matrix(b0 <- rep(1, length(x)))
    fin <- is.finite(x)
    B <- matrix(floor(x))
    r <- as.vector(x) - drop(B)
    len <- 0
    while(any(which <- fin & (r > 1/max.denominator)) &&
          (len <- len + 1) <= cycles) {
      a <- a0
      b <- b0
      a[which] <- 1
      r[which] <- 1/r[which]
      b[which] <- floor(r[which])
      r[which] <- r[which] - b[which]
      A <- cbind(A, a)
      B <- cbind(B, b)
    }
    pq1 <- cbind(b0, a0)
    pq <- cbind(B[, 1], b0)
    len <- 1
    while((len <- len + 1) <= ncol(B)) {
      pq0 <- pq1
      pq1 <- pq
      pq <- B[, len] * pq1 + A[, len] * pq0
    }
    pq[!fin, 1] <- x[!fin]
    list(rat = pq, x = x)
  }
  ans <- .rat(x, cycles, max.denominator)
  ndc <- paste(ans$rat[, 1], ans$rat[, 2], sep = "/")
  int <- ans$rat[, 2] == 1
  ndc[int] <- as.character(ans$rat[int, 1])
  structure(ans$x, fracs = ndc, class = c("fractions", class(ans$x)))
}
