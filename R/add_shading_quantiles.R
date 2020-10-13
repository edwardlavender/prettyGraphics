#' @title Add shading for the quantiles of observed variation to a plot
#' @description This function adds shading to a plot to mark the quantiles of observed variation. Under the default settings, quantiles near the middle of a distribution are shaded more darkly than quantiles towards the edge of the distribution.
#' @param x A numeric vector.
#' @param probs A numeric vector of probabilities.
#' @param cols A character vector of colours. This should be of the same length as \code{probs}.
#' @param horiz A logical input that defines whether or not shading is added horizontally or vertically across a plot (i.e., whether \code{x} is the x or y variable on a given plot). This is passed to \code{\link[prettyGraphics]{add_shading_bar}}.
#' @param lim A numeric input that defines the horizontal or vertical limits. This is passed to passed to \code{\link[prettyGraphics]{add_shading_bar}}.
#' @param return_list A logical input that defines whether or not to return a list of outputs (see Value).
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{add_shading_bar}}.
#' @return This function adds shading to a plot to show the sample quantiles corresponding to inputted probabilities. A named list of outputs is also returned if \code{return_list = TRUE}. This contains: (1) 'dat', a dataframe with probabilities ('prob'), colours ('col') and quantiles ('quantile'); (2) 'data_legend', a dataframe that contains only probabilities ('x') and colours ('col'); and (3) 'pretty_axis_args', a list of suggested arguments for pretty axes. 'data_legend' and 'pretty_axis_args' can be passed directly to \code{\link[prettyGraphics]{add_colour_bar}} to add a colour bar to the plot.
#'
#' @examples
#' # Simulate some normally distributed observations
#' n <- 100
#' x <- stats::rnorm(100, -200, 200)
#' pp <- par(oma = c(2, 2, 2, 4))
#'
#' # Create a blank plot with appropriate axis limits
#' axis_ls <- pretty_plot(x, type = "n", return_list = TRUE)
#'
#' # Add shading for the quantiles of observations, saving the
#' # ... list returned by the function so we can add a colour bar later.
#' asq_ls <- add_shading_quantiles(x,
#'                                 horiz = TRUE,
#'                                 lim = axis_ls[[1]]$lim,
#'                                 border = NA,
#'                                 return_list = TRUE)
#'
#' # Add observed points onto the plot
#' points(1:n, x)
#'
#' # Add a colour bar via TeachingDemos::subplot() and
#' # ... prettyGraphics::add_colour_bar()
#' # ... using outputs returned by function:
#' TeachingDemos::subplot(fun =
#'                          add_colour_bar(
#'                            data_legend = asq_ls$data_legend,
#'                            pretty_axis_args = asq_ls$pretty_axis_args
#'                          ),
#'                        x = axis_ls[[1]]$lim[2] + 1,
#'                        y = axis_ls[[2]]$lim[1],
#'                        size = c(0.2, 2.8),
#'                        vadj = 0, hadj = 0
#' )
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'

add_shading_quantiles <-
  function(x,
           probs = seq(0, 1, length.out = 20),
           cols = scales::alpha("dimgrey", ifelse(probs > 0.5, 1-probs, probs) + 0.05),
           horiz = TRUE,
           lim,
           return_list = TRUE,...){

    #### Define dataframe
    dat <- data.frame(prob = probs, col = cols)
    dat$quantile <- as.numeric(stats::quantile(x, prob = dat$prob))

    #### Add shading
    add_shading_bar(dat$quantile[1:(nrow(dat)-1)],
                    dat$quantile[2:nrow(dat)],
                    horiz = horiz,
                    lim = lim,
                    col = dat$col[1:(nrow(dat)-1)],...) %>% invisible()

    #### Define outputs
    if(return_list){
      ## Dataframe for add_colour_bar()
      data_legend <- data.frame(x = dat$prob, col = dat$col)
      ## Suggested pretty_axis_args for add_colour_bar()
      pretty_axis_args <- list("4" = NULL)
      pretty_axis_args[[1]]$axis <- list(side = 4,
                                         at = seq(0, 1, by = 0.25),
                                         pos = 1,
                                         las = TRUE)
      pretty_axis_args[[1]]$lim <- c(0, 1)
      attributes(pretty_axis_args[[1]]$lim)$user <- c(FALSE, FALSE)
      out <- list(dat = dat,
                  data_legend = data_legend,
                  pretty_axis_args = pretty_axis_args)
      return(out)
    }

  }

