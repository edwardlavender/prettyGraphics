#######################################
#######################################
#### pretty_cols_brewer()

#' @title Create pretty colour schemes
#' @description This function is a simple wrapper for \code{\link[RColorBrewer]{brewer.pal}} and \code{\link[grDevices]{colorRampPalette}} or other colour palette functions that generates pretty colour schemes.
#' @param zlim A numeric range that the colour scheme should span.
#' @param scheme A character that defines a colour scheme supported by \code{\link[RColorBrewer]{brewer.pal}}.
#' @param select An integer vector that defines which colours to draw from the \code{scheme}.
#' @param pal A colour palette function. If supplied \code{scheme} and \code{select} are silently ignored.
#' @param n_breaks The number of breaks in the colour scheme.
#' @param rev A logical value that defines whether or not to reverse sampled colours.
#' @details This function is designed to work in conjunction with \code{\link[fields]{image.plot}}.
#' @return The function returns a named list that contains the z limits (`zlim'), a numeric vector of breaks (`breaks'), the colour palette function (`pal') and the associated vector of colours (`col'). This can be passed to \code{\link[fields]{image.plot}}.
#' @seealso \code{\link[prettyGraphics]{pretty_cols_split_heat}}
#' @author Edward Lavender
#' @export

pretty_cols_brewer <- function(zlim,
                               scheme = "YlOrRd",
                               select = 1:8,
                               pal = NULL,
                               n_breaks = 100,
                               rev = FALSE){
  if(!requireNamespace("RColorBrewer", quietly = TRUE)){
    stop("Package \"RColorBrewer\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  breaks <- seq(zlim[1], zlim[2], length.out = n_breaks)
  if(is.null(pal)){
    n <- max(select)
    cols <- RColorBrewer::brewer.pal(n, scheme)
    cols <- cols[select]
    pal <- grDevices::colorRampPalette(cols)
  }
  cols <- pal(n_breaks - 1)
  if(rev) cols <- rev(cols)
  out <- list(zlim = zlim,
              breaks = breaks,
              pal = pal,
              col = cols)
  return(out)
}


#######################################
#######################################
#### pretty_cols_split_heat()

#' @title Create the `split-heat' colour scheme
#' @description This function creates the `split-heat' colour scheme. For a user-defined range (\code{zlim}) that spans a `splitting' value (i.e., \code{split = 0}), the function returns a dichotomous vector of colours that distinguishes values either side of that value. By default, blue colours are returned for values below \code{split} (e.g., negative numbers) and red colours are returned for values above \code{split} (e.g., positive numbers).
#'
#' @param zlim A numeric range that the colour scheme should span.
#' @param split A number that divides the colour scheme. The default is \code{split = 0}, which splits the colour scheme around 0.
#' @param n_breaks The number of breaks in the colour scheme.
#' @param scheme_cold A character that defines the colour scheme for numbers below \code{split} (e.g., negative numbers), supported by \code{\link[RColorBrewer]{brewer.pal}}.
#' @param scheme_hot A character that defines the colour scheme for numbers above \code{split} (e.g., positive numbers), supported by \code{\link[RColorBrewer]{brewer.pal}}.
#' @param select_cold,select_hot Integer vectors that define which colours to draw from \code{scheme_cold} and \code{scheme_cold} respectively.
#'
#' @details This function is designed to work in conjunction with \code{\link[fields]{image.plot}}.
#' @return The function returns a named list that contains the z limits (`zlim'), a numeric vector of breaks (`breaks') and the associated vector of colours (`col'). This can be passed to \code{\link[fields]{image.plot}}.
#'
#' @examples
#' # Generate a raster with positive and negative numbers
#' nrw <- ncol <- 5
#' n <- nrw * ncol
#' zlim <- c(-0.1, 0.1)
#' m <- matrix(runif(n, zlim[1], zlim[2]), nrow = nrw, ncol = ncol)
#' r <- raster::raster(m)
#' # Define colours
#' col_param <- pretty_cols_split_heat(zlim = zlim, n_breaks = 100)
#' # Visualise raster with colour scheme
#' fields::image.plot(r,
#'                    zlim = col_param$zlim,
#'                    breaks = col_param$breaks,
#'                    col = col_param$col)
#' @seealso \code{\link[prettyGraphics]{pretty_cols_brewer}}
#' @author Edward Lavender
#' @export
#'

pretty_cols_split_heat <- function(zlim,
                                   split = 0,
                                   scheme_cold = "Blues",
                                   scheme_hot = "Reds",
                                   select_cold = 1:8,
                                   select_hot = 1:8,
                                   n_breaks = 100){
  if(!requireNamespace("RColorBrewer", quietly = TRUE)){
    stop("Package \"RColorBrewer\" is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(!(zlim[1] < split) | !(zlim[2] > split)){
    stop("zlim[1] must be less than 'split' and zlim[2] must be more than 'split'.")
  }
  # Define a sequence of breaks
  breaks_negative  <- seq(zlim[1], split, length.out = n_breaks/2)
  by <- breaks_negative[2] - breaks_negative[1]
  breaks_positive <- seq(split + by, zlim[2], length.out = n_breaks/2)
  breaks <- c(breaks_negative, breaks_positive)
  # Colour palette for negative numbers
  cols_negative <- RColorBrewer::brewer.pal(max(select_cold), scheme_cold)
  cols_negative <- cols_negative[select_cold]
  cols_negative <- rev(cols_negative)
  pal_negative  <- grDevices::colorRampPalette(cols_negative)
  # Colour palette for positive numbers
  cols_positive <- RColorBrewer::brewer.pal(max(select_hot), scheme_hot)
  cols_positive <- cols_positive[select_hot]
  pal_positive  <- grDevices::colorRampPalette(cols_positive)
  # Define colours
  n_scheme_cold <- length(breaks_negative) - 1
  n_hot <- length(breaks_positive)
  cols <- c(pal_negative(n_scheme_cold), pal_positive(n_hot))
  # Return list of outputs
  out <- list(zlim = zlim,
              breaks = breaks,
              col = cols)

  return(out)
}


