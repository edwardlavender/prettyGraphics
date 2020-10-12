#' @title Add shading for the quantiles of observed variation to a plot
#' @description This function adds shading to a plot to mark the quantiles of observed variation. Under the default settings, quantiles near the middle of a distribution are shaded more darkly than quantiles towards the edge of the distribution.
#' @param x A numeric vector.
#' @param probs A numeric vector of probabilities.
#' @param cols A character vector of colours. This should be of the same length as \code{probs}.
#' @param horiz A logical input that defines whether or not shading is added horizontally or vertically across a plot (i.e., whether \code{x} is the x or y variable on a given plot). This is passed to \code{\link[prettyGraphics]{add_shading_bar}}.
#' @param lim A numeric input that defines the horizontal or vertical limits. This is passed to passed to \code{\link[prettyGraphics]{add_shading_bar}}.
#' @param return_dat A logical input which defines whether or not to return a dataframe with probabilities, colours and quantiles.
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{add_shading_bar}}.
#' @return This function adds shading to a plot to show to the sample quantiles corresponding to inputted probabilities. A dataframe with probabilities ('prob'), colours ('col') and quantiles ('quantile') is also returned if \code{return_dat} is \code{TRUE}.
#' @examples
#' # Simulate some normally distributed observations
#' n <- 100
#' x <- stats::rnorm(100, -200, 200)
#' axis_ls <- pretty_plot(x, type = "n", return_list = TRUE)
#' # Add shading for the quantiles of observations
#' add_shading_quantiles(x, horiz = TRUE, lim = axis_ls[[1]]$lim, border = NA)
#' points(1:n, x)
#' @author Edward Lavender
#' @export
#'

add_shading_quantiles <-
  function(x,
           probs = seq(0, 1, length.out = 20),
           cols = scales::alpha("dimgrey", ifelse(probs > 0.5, 1-probs, probs) + 0.05),
           horiz = TRUE,
           lim,
           return_dat = TRUE,...){

    #### Define dataframe
    dat <- data.frame(prob = probs, col = cols)
    dat$quantile <- as.numeric(stats::quantile(x, prob = dat$prob))

    #### Add shading
    add_shading_bar(dat$quantile[1:(nrow(dat)-1)],
                    dat$quantile[2:nrow(dat)],
                    horiz = horiz,
                    lim = lim,
                    col = dat$col[1:(nrow(dat)-1)],...) %>% invisible()

    #### Return dat
    if(return_dat) return(dat)
  }

