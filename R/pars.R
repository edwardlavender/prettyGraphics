######################################
######################################
#### par_mf()

#' @title Define \code{\link[graphics]{par}}'s \code{mfrow} or \code{mfcol} arguments to fit \code{n} plots on a single page
#' @description For a given number of plots, this function defines a vector of two values for the \code{mfrow} (or \code{mfcol}) arguments of \code{\link[graphics]{par}} such that all plots can fit on a single page. This is particularly useful if you are producing plots for multiple groups (e.g. individuals), each of which contains a varying number of subplots: in this scenario, you do not have to manually specify \code{mfrow} or \code{mfcol} for each plot; you can simply pass the number of plots to \code{plot_mf()}, inside \code{\link[graphics]{par}}, to define a suitable, case-specific layout.
#'
#' @param n An integer which defines the number of plots to be produced on a single page.
#'
#' @source This function was inspired by the implementation of the \code{pages} argument in \code{\link[mgcv]{plot.gam}}.
#'
#' @return The function returns a vector comprising two integers, which can be passed to the \code{mfrow} (or \code{mfcol}) arguments of \code{\link[graphics]{par}} to plot \code{n} plots on a single page.
#'
#' @examples
#' #### Example (1): Implement par_mf() for n plots
#' par_mf(10)
#' par_mf(20)
#' par_mf(23)
#'
#' #### Example (2): Use par_mf() to define par()'s mfrow argument:
#' pp <- par(mfrow = par_mf(3))
#' plot(1, 2); plot(2, 4); plot(8, 9)
#' par(pp)
#'
#' #### Example (3): Use par_mf() to define par()'s mfcol argument in the same way:
#' pp <- par(mfcol = par_mf(3))
#' plot(1, 2); plot(2, 4); plot(8, 9)
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

par_mf <- function(n){
  n <- as.integer(n)
  c <- r <- trunc(sqrt(n))
  if (c < 1)
    r <- c <- 1
  if (c * r < n)
    c <- c + 1
  if (c * r < n)
    r <- r + 1
  return(c(r, c))
}


######################################
######################################
#### par_tri()

#' @title Plot the lower/upper triangle of a multi-plot square matrix
#' @description For a multi-plot square matrix (i.e., a multi-panel plot comprising a matrix of subplots for each combination of units, e.g., individuals), it can be useful to plot only the lower or upper triangle of the matrix to avoid visualisation of duplicate combinations. For a given square matrix, this function returns the indices of plots that lie along the lower or upper matrix.
#'
#' @param mf A vector of two numbers that specify the number of subplots in each direction in a multi-panel plot (i.e., an input to \code{\link[graphics]{par}}'s \code{mfrow} or \code{mfcol} argument).
#' @param type A character vector (\code{"upper.tri"} or \code{"lower.tri"}) which specifies whether or not to return an index of subplots in the upper triangle or lower triangle. For plots filled by row, then column, this works as expected; for plots filled by column, then by row, \code{"upper.tri"} returns the lower triangle and \code{"lower.tri"} returns the upper triangle (see Examples).
#'
#' @return This function returns the indices of plots along the lower or upper triangle of a multi-panel figure.
#'
#' @examples
#' #### Example (1): Plot the upper triangle with par(mfrow = ...)
#' pp <- par(mfrow = c(4, 4))
#' for(i in 1:16){
#'   if(i %in% par_tri(mf = c(4, 4), type = "upper.tri")) plot(1) else plot.new()
#' }
#' par(pp)
#'
#' #### Example (2): Plot the lower triangle with par(mfrow = ...)
#' pp <- par(mfrow = c(4, 4))
#' for(i in 1:16){
#'   if(i %in% par_tri(mf = c(4, 4), type = "lower.tri")) plot(1) else plot.new()
#' }
#' par(pp)
#'
#' #### Example (3): Plot the upper triangle with par(mfcol = ...)
#' pp <- par(mfcol = c(4, 4))
#' for(i in 1:16){
#'   if(i %in% par_tri(mf = c(4, 4), type = "lower.tri")) plot(1) else plot.new()
#' }
#' par(pp)
#'
#' #### Example (4): Plot the lower triangle with par(mfcol = ...)
#' pp <- par(mfcol = c(4, 4))
#' for(i in 1:16){
#'   if(i %in% par_tri(mf = c(4, 4), type = "upper.tri")) plot(1) else plot.new()
#' }
#' par(pp)
#'
#' @author Edward Lavender
#' @export

par_tri <- function(mf, type = "upper.tri"){

  #### Checks
  # mf argument should compose two elements
  stopifnot(length(mf) == 2)
  # a square matrix is required
  stopifnot(mf[1] == mf[2])
  # check frunction inputs
  type <- check_input(arg = "type", input = type, supp = c("lower.tri", "upper.tri"), default = "upper.tri")

  #### Preprocessing
  if(type == "lower.tri") type_foo <- lower.tri else if(type == "upper.tri") type_foo <- upper.tri

  #### Define positions of plots along lower or upper diagonal
  mat <- matrix(1:(mf[1]*mf[2]), mf[1], mf[2], byrow = TRUE)
  index <- sort(mat[type_foo(mat, diag = TRUE)])

  #### Return outputs
  return(index)

}


#### End of code.
##########################################
##########################################
