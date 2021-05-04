#' @title Pretty two-dimensional predictions
#' @description This is function plots pretty two-dimensional predictions from a statistical model.
#' @param x A model (e.g. an output from \code{\link[mgcv]{gam}}).
#' @param view A character vector of two variables that define the variables on the x and y axis.
#' @param n_grid,cond,predict_param,select Prediction controls.
#'   \itemize{
#'     \item \code{n_grid} is an integer that defines the resolution of the surface (in both x and y directions).
#'     \item \code{cond} (optional) is a named list that defines the values of other predictors (i.e., those not in \code{view}) for which to make predictions. If un-supplied, factor variables are set at the most commonly occuring factor level and continuous variables are set at the closest observed value to the median.
#'     \item \code{predict_param} (optional) A named list of arguments, passed to \code{\link[stats]{predict}}, to customise predictions.
#'     \item \code{select} (optional) If the call to \code{\link[stats]{predict}} returns a list, \code{select} is the name of the element in that list that is plotted (e.g., \code{"fit"} or \code{"se.fit"}). If the call to \code{\link[stats]{predict}} returns a numeric vector, this is ignored.
#'   }
#' @param col_pal,col_n Colour controls.
#'   \itemize{
#'     \item \code{col_pal} is a colour palette function from which colours are drawn.
#'     \item \code{col_n} is the number of colours to use in the colour scheme.
#'   }
#' @param xlab,ylab X and y axis labels.
#' @param xlim,ylim,zlim Axis limits.
#' @param pretty_axis_args A named list of arguments, passed to \code{\link[prettyGraphics]{pretty_axis}}, to control axes.
#' @param add_xy A named list of arguments, passed to \code{\link[graphics]{points}}, to add observations to the plot. \code{add_xy = NULL} suppresses this option, \code{add_xy = list()} implements default arguments and a named list customises these.
#' @param add_rug_x,add_rug_y Named list of arguments, passed to \code{\link[graphics]{rug}}, to add observed values of the variables defined in \code{view} to the plot. \code{add_rug_* = NULL} suppresses this option, \code{add_rug_*} implements default arguments and a named list customises these.
#' @param add_contour A named list of arguments, passed to \code{\link[graphics]{contour}}, to add contour lines to the plot. \code{add_contour = NULL} suppresses this option, \code{add_contour = list()} implements default arguments and a named list customises these.
#' @param add_legend,legend_breaks,legend_labels,legend_x,legend_y Legend controls.
#'   \itemize{
#'     \item \code{add_legend} A named list of arguments, passed to \code{\link[prettyGraphics]{add_colour_bar}}, to add a legend to the plot. \code{add_legend = NULL} suppresses this option, \code{add_legend = list()} implements default arguments and a named list customises these.
#'     \item \code{legend_breaks} and \code{legend_labels} are functions that modify the legend breaks and legend labels respectively.
#'     \item \code{legend_x} and \code{legend_y} are numeric vectors of length two that specify the x and y positions, on the current plot, for the four corners of the legend. If un-supplied, the two \code{legend_x} coordinates are taken as the maximum x limit of x plus (a) 1 per cent and (b) 10 per cent of the difference between the maximum and minimum x limits. \code{legend_y} is taken from \code{ylim}.
#'   }
#' @param ... Additional arguments passed to \code{\link[graphics]{image}}, which is used for plotting (excluding \code{col}, \code{breaks} and \code{axes} which are defined internally).
#'
#' @details This function was motivated by \code{\link[mgcv]{vis.gam}} (see also \code{\link[prettyGraphics]{pretty_smooth_2d}}).
#'
#' @return The function returns a contour plot of the predictions of a model for the two variables defined in \code{view} and, invisibly, a named list containing the prediction matrix (`z') and the list of pretty axis parameters produced by \code{\link[prettyGraphics]{pretty_axis}} (`axis_ls').
#'
#' @examples
#' #### Simulate example data and fit model (following ?mgcv::vis.gam examples)
#' set.seed(0)
#' n    <- 200
#' sig2 <- 4
#' x0   <- runif(n, 0, 1)
#' x1   <- runif(n, 0, 1)
#' x2   <- runif(n, 0, 1)
#' y    <- x0^2 + x1 * x2 + runif(n, -0.3, 0.3)
#' g    <- mgcv::gam(y ~ s(x0, x1, x2))
#'
#' #### Example (1): Contour plot using default options
#' pp <- par(oma = c(2, 2, 2, 10))
#' pretty_predictions_2d(g, view = c("x1", "x2"))
#'
#' #### Example (2): Customise predictions
#' # Use n_grid to control the grid resolution
#' pretty_predictions_2d(g, view = c("x1", "x2"), n_grid = 10)
#' # Use cond to set other variables at specific values
#' pretty_predictions_2d(g, view = c("x1", "x2"), cond = list(x0 = mean(x0)))
#' # Use predict_param for further control, e.g., to plot SEs
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       predict_param = list(se.fit = TRUE), select = "se.fit")
#'
#' #### Example (3): Customise colours
#' # Use col_pal and col_n
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       col_pal = grDevices::heat.colors,
#'                       col_n = 10)
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       col_pal = grDevices::heat.colors,
#'                       col_n = 100)
#'
#' #### Example (4): Customise axes via xlim, ylim and pretty_axis_args
#' # Use xlim and ylim
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       xlim = c(0, 1),
#'                       ylim = c(0, 1))
#' # Use pretty_axis_args
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       pretty_axis_args = list(side = 1:4))
#'
#' #### Example (5): Add observed data
#' # Specify list() to use default options
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       add_xy = list())
#' # Customise addition of observed data
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       add_xy = list(pch = ".'", cex = 5))
#'
#' #### Example (6): Add rugs for the x and y variables
#' # Use default options
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       add_rug_x = list(),
#'                       add_rug_y = list())
#' # Customise options
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       add_rug_x = list(col = "grey"),
#'                       add_rug_y = list(col = "grey"))
#'
#' #### Example (7): Add contours
#' # Use default options
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       add_contour = list())
#' # Customise contours
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       add_contour = list(labcex = 1.5))
#'
#' #### Example (8): Add add_colour_bar()
#' # Use default options
#' pp <- graphics::par(oma = c(2, 2, 2, 10))
#' pretty_predictions_2d(g,
#'                       view = c("x1", "x2"),
#'                       add_contour = list(labcex = 1.5),
#'                       add_legend = list())
#' graphics::par(pp)
#' # Customise colour bar
#' pp <- graphics::par(oma = c(2, 2, 2, 10))
#' pretty_predictions_2d(g,
#'                       view = c("x1", "x2"),
#'                       add_contour = list(labcex = 1.5),
#'                       zlim = c(-1, 2), add_legend = list())
#' graphics::par(pp)
#' # E.g., reverse the colour scheme and legend
#' # ... This is useful if, for example, the surface represents the depth of an
#' # ... animal, in which case it is natural to have shallower depths near the
#' # ... top of the legend.
#' pp <- graphics::par(oma = c(2, 2, 2, 10))
#' pretty_predictions_2d(g, view = c("x1", "x2"),
#'                       col_pal = function(n) rev(viridis::viridis(n)),
#'                       add_contour = list(labcex = 1.5),
#'                       add_legend = list(),
#'                       legend_breaks = function(x) x *-1,
#'                       legend_labels = abs)
#' graphics::par(pp)

#' @author Edward Lavender
#' @export
#'

pretty_predictions_2d <- function(x, view = NULL,
                                  n_grid = 30, cond = list(), predict_param = list(), select = "fit",
                                  xlim = NULL, ylim = NULL, zlim = NULL,
                                  xlab = NULL, ylab = NULL,
                                  pretty_axis_args = list(side = 1:4, axis = list(list(), list(), list(labels = FALSE),
                                                                                  list(labels = FALSE))),
                                  col_pal = viridis::viridis, col_n = 100,
                                  add_xy = NULL,
                                  add_rug_x = NULL, add_rug_y = NULL,
                                  add_contour = NULL,
                                  add_legend = NULL,
                                  legend_breaks = NULL, legend_labels = NULL,
                                  legend_x = NULL, legend_y = NULL,...){

  #### Define data for predictions
  dat <- stats::model.frame(x)
  terms <- all.vars(stats::formula(x))[-1]
  if(length(view) == 0L) view <- terms[1:2]
  terms <- terms[!(terms %in% view)]
  xp <- seq(min(dat[, view[1]]), max(dat[, view[1]]), length.out = n_grid)
  yp <- seq(min(dat[, view[2]]), max(dat[, view[2]]), length.out = n_grid)
  nd <- expand.grid(x = xp, y = yp)
  colnames(nd) <- view
  if(length(terms) > 0){
    for(term in terms){
      if(term %in% names(cond)) {
        nd[, term] <- cond[[term]]
      } else {
        if(is.factor(dat[, term])) {
          nd[, term] <- names(which.max(table(dat[, term])))
        } else{
          nd[, term] <- dat[, term][which.min(abs(dat[, term] - stats::median(dat[, term])))]
        }
      }
    }
  }

  #### Make predictions
  predict_param$object <- x
  if(is.null(predict_param$newdata)) predict_param$newdata <- nd
  preds <- do.call(stats::predict, predict_param)
  if(is.list(preds)) preds <- preds[[select]]
  nd$pred <- preds
  z <- matrix(NA, n_grid, n_grid)
  s <- seq(0, nrow(nd), by = n_grid)
  for(i in seq_along(s)[-length(s)]) z[, i] <- nd$pred[(s[i] + 1):s[i+1]]

  #### Define axis limits
  if(is.null(pretty_axis_args$lim)) pretty_axis_args$lim <- list(x = NULL, y = NULL)
  if(is.null(xlim)) {
    xlim <- pretty_axis_args$lim[[1]]
    if(is.null(xlim)) xlim <- range(xp)
  }
  if(is.null(ylim)) {
    ylim <- pretty_axis_args$lim[[2]]
    if(is.null(ylim)) ylim <- range(yp)
  }
  if(is.null(zlim)) zlim <- range(z)

  #### Define colours
  col_param <- pretty_cols_brewer(zlim = zlim,
                                  pal = col_pal,
                                  n_breaks = col_n + 1)

  #### Plot graph, with pretty axes
  if(is.null(xlab)) xlab <- view[1]
  if(is.null(ylab)) ylab <- view[2]
  graphics::image(xp, yp, z,
                  xlab = xlab, ylab = ylab,
                  xlim = xlim, ylim = ylim, zlim = col_param$zlim,
                  breaks = col_param$breaks, col = col_param$col,
                  axes = FALSE,...)

  #### Add observed data
  if(!is.null(add_xy)) {
    add_xy$x <- dat[, view[1]]
    add_xy$y <- dat[, view[2]]
    do.call(graphics::points, add_xy)
  }

  #### Add contours
  if(!is.null(add_contour)){
    add_contour$x <- xp
    add_contour$y <- yp
    add_contour$z <- z
    add_contour$add <- TRUE
    do.call(graphics::contour, add_contour)
  }

  #### Add rugs
  # x variable
  if(!is.null(add_rug_x)) {
    if(is.null(add_rug_x$x)) add_rug_x$x <- dat[, view[1]]
    if(is.null(add_rug_x$side)) add_rug_x$side <- 1
    if(is.null(add_rug_x$pos)) {
      if(add_rug_x$side == 1) {
        add_rug_x$pos <- ylim[1]
      } else if(add_rug_x$side == 3) {
        add_rug_x$pos <- ylim[2]
      } else {
        stop("add_rug_x$side should be 1 or 3.")
      }
    }
    do.call(graphics::rug, add_rug_x)
  }
  # y variable
  if(!is.null(add_rug_y)) {
    if(is.null(add_rug_y$x)) add_rug_y$x <- dat[, view[2]]
    if(is.null(add_rug_y$side)) add_rug_y$side <- 2
    if(is.null(add_rug_y$pos)) {
      if(add_rug_y$side == 2) {
        add_rug_y$pos <- xlim[1]
      } else if(add_rug_y$side == 3) {
        add_rug_y$pos <- xlim[2]
      } else {
        stop("add_rug_y$side should be 2 or 4.")
      }
    }
    do.call(graphics::rug, add_rug_y)
  }

  #### Add axes
  axis_ls <- implement_pretty_axis_args(x = list(xp, yp),
                                        pretty_axis_args = pretty_axis_args,
                                        xlim = xlim, ylim = ylim,...)
  pretty_axis(axis_ls = axis_ls, add = TRUE)

  #### Add legend
  if(!is.null(add_legend)){

    ## Define legend param
    # Legend position
    if(is.null(legend_x)){
      legend_x <- rep(xlim[2], 2)
      legend_x[1] <- legend_x[1] + 0.01 * abs(xlim[1] - xlim[2])
      legend_x[2] <- legend_x[2] + 0.1 * abs(xlim[1] - xlim[2])
    }
    if(is.null(legend_y)) legend_y <- ylim
    # Legend data
    if(is.null(add_legend$data_legend)) {
      add_legend$data_legend <- data.frame(x = col_param$breaks[1:(length(col_param$breaks) - 1)],
                                           col = col_param$col)
    }
    if(!is.null(legend_breaks)) {
      add_legend$data_legend$x <- legend_breaks(add_legend$data_legend$x)
    }
    # Legend axis param
    add_legend_paa <- list(side = 4,
                           x = list(add_legend$data_legend$x),
                           axis = list(pos = 1),
                           lim = list(range(add_legend$data_legend$x)))
    add_legend$pretty_axis_args <- list_merge(add_legend_paa, add_legend$pretty_axis_args)
    # Implement pretty axis args
    add_legend$axis_ls <- implement_pretty_axis_args(x = list(add_legend$data_legend$x),
                                                     pretty_axis_args = add_legend$pretty_axis_args,...)
    # Customise label
    if(!is.null(legend_labels)) {
      add_legend$axis_ls[[1]]$axis$labels <-
        legend_labels(add_legend$axis_ls[[1]]$axis$at)
      n_digits <- add_legend$pretty_axis_args$control_digits
      pi_note  <- add_legend$pretty_axis_args$pi_notation
      sci_note <- add_legend$pretty_axis_args$control_sci_notation
      add_legend$axis_ls[[1]]$axis$labels <-
        pretty_labels(x = add_legend$axis_ls[[1]]$axis$labels,
                      at = add_legend$axis_ls[[1]]$axis$labels,
                      n = n_digits,
                      pi_notation_args = pi_note,
                      sci_notation_args = sci_note)
    }
    ## Add legend
    TeachingDemos::subplot(add_colour_bar(add_legend$data_legend,
                                          pretty_axis_args = add_legend$axis_ls),
                           x = legend_x, y = legend_y)

  }

  #### Return outputs
  out <- list(# predict_param = predict_param,
              z = z,
              axis_ls = axis_ls,
              legend = add_legend)
  return(invisible(out))
}
