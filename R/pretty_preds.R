#######################################
#######################################
#### Helpers

#' @title Get model terms
#' @noRd

model_terms <- function(model) {
  # For some models (e.g., mgcv with family gaulss),
  # ... the model formula may be a list
  # ... hence the use of this function to get model terms.
  sfm <- stats::formula(model)
  if (inherits(sfm, "formula")) {
    v <- all.vars(sfm)
  } else if (inherits(sfm, "list")) {
    v <- unique(unlist(sapply(sfm, all.vars)))
  } else {
    stop("Class returned by stats::formula(model) unsupported.", call. = FALSE)
  }
  v
}


#######################################
#######################################
#### pretty_predictions_1d()

#' @title Pretty one-dimensional predictions
#' @description This function plots pretty one-dimensional predictions from a statistical \code{model}. Given a \code{model}, for each predictor, the function plots the predicted values of the response and associated 95 percent confidence intervals. Other predictors are held at the first level (for factors) or an average (e.g., mean or median, as specified) value (for doubles) or at custom values specified in a dataframe called \code{newdata}.
#'
#' @param model A model (e.g. an output from \code{\link[mgcv]{gam}}).
#' @param data (optional) The dataframe used to fit the model. If missing, this is extracted via \code{model.frame(model)}; however, this approach may fail if functions (e.g., \code{\link[base]{scale}}) have been applied to variables as part of the model formula.
#' @param x_var,n_pred,average,newdata,constants (optional) Prediction controls.
#'   \itemize{
#'     \item \code{x_var} is a character variable that defines the name(s) of  predictors for which to plot predictions. If unsupplied, predictions are plotted for each predictor in the \code{model}.
#'     \item \code{n_pred} is a number that defines, for continuous predictors, the prediction resolution. For each continuous predictor, a sequence of values from the minimum to maximum value for that variable, with \code{n_pred} elements, is used for prediction. (For factors, predictions are plotted for each factor level.) Alternatively, \code{newdata} can be supplied.
#'     \item \code{average} is a function that is used to define the value at which doubles are held constant for prediction. The default is \code{\link[base]{mean}}.
#'     \item \code{newdata} is a dataframe that contains the data used for prediction. If supplied, this should contain one variable that changes in value (defined in \code{x_var}), with other variables held at selected values. If supplied, this supersedes \code{constants} (see below).
#'     \item \code{constants} is a one-row dataframe of constant values for explanatory variables. If supplied, \code{n_pred} model predictions are generated for \code{x_var}, while holding other variables constant at the values specified in \code{constants}.
#'     }
#' @param extract_fit,extract_se Functions that extract fitted values and standard errors, respectively, from the object returned by \code{\link[stats]{predict}}.
#' @param transform_x A function to transform values of the predictor(s) for plotting.
#' @param xlim,ylim,ylim_fix,pretty_axis_args Axis controls. \code{xlim} and \code{ylim} control axis limits for all plots. If unsupplied, pretty limits are used. If pretty limits are used, \code{ylim_fix} is a logical variable that defines whether or not to use the same y-axis limits on all plots (\code{TRUE}), which can facilitate comparisons, or to use plot-specific limits (\code{FALSE}). \code{pretty_axis_args} is a named list of arguments, passed to \code{\link[prettyGraphics]{pretty_axis}}, for further control.
#' @param add_points (optional) A named list of arguments, passed to \code{\link[graphics]{points}}, to customise observations added to plots. An empty list specifies default options. \code{NULL} suppresses this argument.
#' @param add_error_bars,add_error_envelope (optional) Named lists of arguments, passed to \code{\link[prettyGraphics]{add_error_bars}} and \code{\link[prettyGraphics]{add_error_envelope}}, to customise the appearance of error bars (for factor predictors) or envelopes (for continuous predictors) respectively. Empty lists specify default options. \code{NULL} suppresses these arguments.
#' @param add_order A character vector that defines the order in which you want to add \code{predictions} and \code{points}. By default, predictions are added first, since these often mask points otherwise. However, this order is reversible.
#' @param add_xlab,add_ylab,add_main (optional) Named lists of arguments, passed to \code{\link[graphics]{mtext}}, to add axis titles to plots. X-axis labels and plot titles are added to each plot, while only one global y-axis label is added. Empty lists specify default arguments, in which case variable names are taken as specified in the \code{model} and plot titles are given as capitalised letters or numbers in square brackets (if there are more than 26 predictors). Alternatively, names can be specified via the `text' argument to \code{\link[graphics]{mtext}}. For \code{add_xlab} and \code{add_main} , the `text' argument can be a vector with one element for each plot; for \code{add_ylab} only one element should be supplied. \code{NULL} suppress these arguments.
#' @param one_page A logical variable that defines whether or not to plot all plots on one page.
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{pretty_plot}}.
#'
#' @details Interactions are not currently supported.
#'
#' @return The function plots predictions from a model. A list of axis parameters, with one element (from \code{\link[prettyGraphics]{pretty_axis}}) for each variable, is returned invisibly.
#'
#' @examples
#' #### Define a model for predictions
#' mod_1 <- stats::lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
#' summary(mod_1)
#'
#' #### Example (1): Plot predictions using default options
#' pretty_predictions_1d(mod_1)
#'
#' #### Example (2): Plot predictions for specified variables
#' pretty_predictions_1d(mod_1, x_var = c("Sepal.Width"))
#' pretty_predictions_1d(mod_1, x_var = c("Sepal.Width", "Species"))
#'
#' #### Example (3): Plot predictions using custom newdata
#' p_dat <- data.frame(Sepal.Width = median(iris$Sepal.Width),
#'                     Species = factor(levels(iris$Species),
#'                                      levels = levels(iris$Species)))
#' pretty_predictions_1d(mod_1,
#'                       x_var = "Species",
#'                       newdata = p_dat)
#' # Or use constants to use custom constants but standard values for x_var
#' pretty_predictions_1d(mod_1,
#'                       constants = data.frame(Sepal.Width = 3),
#'                       x_var = "Species")
#'
#' #### Example (4): Customise uncertainty
#' pretty_predictions_1d(mod_1,
#'                       add_error_bars = list(cex = 5, bg = "black"),
#'                       add_error_envelope = list(type = "lines"))
#'
#' #### Example (5): Customise axes
#' pretty_predictions_1d(mod_1,
#'                       ylim = c(NA, 10))
#' pretty_predictions_1d(mod_1,
#'                       ylim_fix = FALSE)
#' pretty_predictions_1d(mod_1,
#'                       pretty_axis_args = list(control_digits = 2))
#'
#' #### Example (6): Customise titles
#' pretty_predictions_1d(mod_1,
#'                       add_xlab = list(text = c("Width", "Species"), line = 2),
#'                       add_ylab = list(line = -2),
#'                       add_main = NULL)
#'
#' #### Example (7) Back transformations
#' # The function can be implemented with tranformed variables, but note:
#' # * Transformations must be applied to the dataframe used to fit the model
#' # ... (and not in the model fitting function);
#' # * Transformations should not change variable types (e.g. scale() transforms
#' # ... numbers into matrices, and this is not permitted).
#' # * All numeric variables are affected by transform_x
#'
#' ## (A) Scale variable and plot predictions on transformed scale
#' # Define function to scale numbers that doesn't change variable types
#' scale_num <- function(x) {
#'   y <- scale(x)
#'   attributes(y)$dim <- NULL
#'   y
#' }
#' # Scale Sepal.Width
#' iris$Sepal.Width.S <- scale_num(iris$Sepal.Width)
#' # Implement model
#' mod_2 <- stats::lm(Sepal.Length ~ Sepal.Width.S + Species, data = iris)
#' # Visualise predictions
#' pretty_predictions_1d(mod_2)
#'
#' ## (B) Back-transform predictions
#' unscale <- function(x) {
#'   mu      <- attr(x, "scaled:center")
#'   sigma   <- attr(x, "scaled:scale")
#'   x * sigma + mu
#' }
#' pretty_predictions_1d(mod_2, transform_x = unscale)
#'
#' @seealso \code{\link[prettyGraphics]{pretty_predictions_2d}}
#' @author Edward Lavender
#' @export

pretty_predictions_1d <- function(model, data = NULL,
                                  newdata = NULL, constants = NULL, x_var = NULL, n_pred = 100, average = mean,
                                  extract_fit = function(p) p$fit, extract_se = function(p) p$se.fit,
                                  transform_x = NULL,
                                  xlim = NULL, ylim = NULL, ylim_fix = TRUE, pretty_axis_args = list(),
                                  add_points = list(cex = 0.5, lwd = 0.5, col = "grey20"),
                                  add_error_bars = list(add_fit = list(pch = 3)),
                                  add_error_envelope = list(),
                                  add_order = c("predictions", "points"),
                                  add_xlab = list(line = 2.5),
                                  add_ylab = list(line = 2.5),
                                  add_main = list(adj = 0, font = 2),
                                  one_page = TRUE,...){

  #### Checks
  check...("x", "y", "xlab", "ylab", "type")
  if(!is.null(newdata)){
    if(length(x_var) != 1L)
      stop("If 'newdata' is specified, the predictor should be specified in 'x_var'.", call. = FALSE)
    if(!is.null(constants)) {
      warning("`constants` is ignored if `newdata` is supplied.", immediate. = TRUE, call. = FALSE)
    }
  }
  # Check named lists
  # ...
  #### Define data used to fit model
  # If variable transformations have been applied
  # ... in the model formula, these are retained here.

  if(is.null(data)) data <- stats::model.frame(model)
  data_y <- data[, 1]
  data_x <- data[, 2:ncol(data), drop = FALSE]
  rhs <- model_terms(model)[-1L]
  data_x <- data_x[, colnames(data_x) %in% rhs, drop = FALSE]
  for(i in 1:ncol(data_x)){
    if(inherits(data_x[, i], "character")){
      data_x[, i] <- factor(data_x[, i])
      warning(
        paste0("'", colnames(data_x)[i], "' column coerced from character to factor with level(s): '",
               paste0(levels(data_x[, i]), collapse = "', '"), "'."
        ), immediate. = TRUE, call. = FALSE)
    }
  }
  if (!is.null(constants)) {
    if (nrow(constants) != 1L) {
      stop("`constants` is expected to be a dataframe with one row.", call. = FALSE)
    }
  }
  add_order <- match.arg(add_order, several.ok = TRUE)

  #### Define a character vector of predictors
  if(!is.null(x_var)){
    if(!(all(x_var %in% colnames(data))))
      stop("Not all elements in 'x_var' are found in 'model.frame(model)'.", call. = FALSE)
  } else {
    x_var <- rhs
  }

  #### Define predictions and information required for plotting
  info_by_var <- lapply(1:length(x_var), function(i){

    #### Define variable
    var <- x_var[i]
    var_is_num <- is_number(data[, var], first = TRUE)

    #### Define new data for prediction
    if(!is.null(newdata)){
      nd <- newdata
      x  <- nd[, var]
    } else {
      ## Define x values for selected predictor for prediction
      if(var_is_num){
        x <- seq(min(data[, var], na.rm = TRUE),
                 max(data[, var], na.rm = TRUE),
                 length.out = n_pred)
      } else {
        x <- levels(data[, var])
        x <- factor(x, levels = x)
      }
      ## Define constants for other variables
      # ... These are either as specified
      # ... Or we simply take the average value for numeric variables
      # ... And the 1st level for factors
      if (!is.null(constants)) {
        # Duplicate constants for every value of x, if supplied
        # This assumes that constants has been correctly defined.
        constants <- lapply(seq_len(length(x)), function(i) {
          constants
        }) |> dplyr::bind_rows()
      } else {
        constants <- lapply(1:ncol(data_x), function(i){
          x_tmp <- data_x[, i]
          if(is_number(x_tmp, first = TRUE)){
            x_tmp <- rep(average(x_tmp, na.rm = TRUE), length(x))
          } else {
            x_tmp <- factor(rep(levels(x_tmp)[1], length(x)), levels = levels(data_x[, i]))
          }
          x_tmp <- data.frame(x_tmp)
          colnames(x_tmp) <- colnames(data_x)[i]
          return(x_tmp)
        })
        constants <- do.call(cbind, constants)
        constants <- data.frame(constants)
        colnames(constants) <- colnames(data_x)
      }
      nd <- constants
      nd[, var] <- x
    }

    #### Make predictions on the scale of the response
    pred <- stats::predict(model, nd, type = "response", se.fit = TRUE)
    pred <- list(fit = extract_fit(pred), se.fit = extract_se(pred))
    pred <- list_CIs(pred)
    pred$fit <- as.numeric(pred$fit)
    pred$lowerCI <- as.numeric(pred$lowerCI)
    pred$upperCI <- as.numeric(pred$upperCI)

    #### Variable transformations
    if(!is.null(transform_x) && var_is_num) {
      attributes(x) <- attributes(data[, var])
      x <- transform_x(x)
    }

    #### Define pretty axes
    # Define sides
    paa <- pretty_axis_args
    if(is.null(paa$side)) paa$side <- 1:2
    # Define limits
    if(is.null(paa$lim)){
      paa$lim <- list(x = NULL,
                      y = range(c(pred$lowerCI, data_y, pred$upperCI), na.rm = TRUE))
    }
    if(!is.null(xlim)) paa$lim$x <- NULL
    if(!is.null(ylim)) paa$lim$y <- NULL
    if(length(paa$lim) == 0L) paa$lim <- NULL

    #### Return a list of information required for plotting
    out <- list(var = var,
                x = x,
                var_is_num = var_is_num,
                pred = pred,
                paa = paa,
                ylim = ylim,
                xlim = xlim)
    return(out)
  })

  #### Set plotting window
  if(length(info_by_var) > 1 && one_page){
    pp <- graphics::par(no.readonly = TRUE)
    on.exit(do.call(graphics::par, pp), add = TRUE)
    graphics::par(mfrow = par_mf(length(x_var)))
  }

  #### Define global axis limits
  if(is.null(ylim) && ylim_fix){
    ylims <- unlist(lapply(info_by_var, function(info) info$paa$lim[[2]]))
    ylim <- pretty_axis(side = 1, x = list(ylims), add = FALSE)[[1]]$lim
    info_by_var <- lapply(info_by_var, function(info){
      if(is.null(xlim)){
        info$paa$lim <- NULL
      } else {
        info$paa$lim <- list(x = xlim, y = NULL)
      }
      return(info)
    })
  }

  #### Define plot-specific axis title sides
  if(!is.null(add_xlab) && is.null(add_xlab$side)) add_xlab$side <- 1
  if(!is.null(add_main) && is.null(add_main$side)) add_main$side <- 3

  #### Loop over each predictor and make plot
  axis_ls_by_var <-
    lapply(1:length(info_by_var), function(i){

      ## Extract info
      info       <- info_by_var[[i]]
      var        <- info$var
      x          <- info$x
      var_is_num <- info$var_is_num
      pred       <- info$pred
      paa        <- info$paa

      ## Base plot
      axis_ls <- pretty_plot(x, pred$fit,
                             xlim = xlim, ylim = ylim,
                             xlab = "", ylab = "",
                             pretty_axis_args = paa,
                             type = "n",...)

      ## Define helpers
      # Add error bars/CIs
      add_p <- function() {
        if(var_is_num){
          if(!is.null(add_error_envelope)){
            add_error_envelope$x  <- x
            add_error_envelope$ci <- pred
            do.call("add_error_envelope", add_error_envelope)
          }
        } else {
          if(!is.null(add_error_bars)){
            add_error_bars$x   <- as.integer(x)
            add_error_bars$fit <- pred$fit
            add_error_bars$lwr <- pred$lowerCI
            add_error_bars$upr <- pred$upperCI
            do.call("add_error_bars", add_error_bars)
          }
        }
      }
      # Add observations
      add_o <- function() {
        if(!is.null(add_points)){
          if(!is.null(transform_x) && var_is_num) {
            data[, var] <- transform_x(data[, var])
          }
          add_points$x <- data[, var]
          add_points$y <- data_y
          args <- names(add_points)
          pars <- names(graphics::par())
          args_in_pars <- args[args %in% pars]
          if(length(args_in_pars) > 0L){
            lapply(args_in_pars, function(arg){
              if(length(add_points[[arg]]) != 1){
                if(length(add_points[[arg]]) != nrow(data)){
                  warning(paste0("length(add_points[['", arg, "']]) (n = ",
                                 length(add_points[[arg]]),
                                 ") does not equal nrow(model.frame(model)) (n = ",
                                 nrow(data), ")."),
                          call. = FALSE, immediate. = TRUE)
                }
              }
            })
          }
          do.call(graphics::points, add_points)
        }
      }
      addpo <- list(predictions = add_p, points = add_o)

      ## Add predictions/observations in order
      lapply(add_order, function(nm) {
        do.call(addpo[[nm]], list())
      })

      ## Add back axes for tidiness
      pretty_axis(axis_ls = axis_ls, add = TRUE)

      ## Add plot-specific titles
      if(!is.null(add_xlab)){
        add_x_title <- add_xlab
        if(is.null(add_x_title$text)) {
          add_x_title$text <- var
        } else {
          add_x_title$text <- add_x_title$text[i]
        }
        do.call(graphics::mtext, add_x_title)
      }
      if(!is.null(add_main)){
        add_main_title      <- add_main
        if(is.null(add_main_title$text)) {
          if(length(info_by_var) <= 26){
            add_main_title$text <- LETTERS[i]
          } else {
            add_main_title$text <- paste0("[", i, "]")
          }
        } else {
          add_main_title$text <- add_main_title$text[i]
        }
        do.call(graphics::mtext, add_main_title)
      }
      return(axis_ls)
    })

  ## Add global titles (y axis)
  if(!is.null(add_ylab)){
    if(is.null(add_ylab$text))  add_ylab$text <- colnames(data)[1]
    if(is.null(add_ylab$side))  add_ylab$side <- 2
    if(length(info_by_var) > 1L && is.null(add_ylab$outer)) add_ylab$outer <- TRUE
    do.call(graphics::mtext, add_ylab)
  }

  #### Return invisible()
  return(invisible(axis_ls_by_var))
}


#######################################
#######################################
#### pretty_predictions_2d()

#' @title Pretty two-dimensional predictions
#' @description This is function plots pretty two-dimensional predictions from a statistical model.
#' @param x A model (e.g. an output from \code{\link[mgcv]{gam}}).
#' @param view A character vector of two variables that define the variables on the x and y axis.
#' @param n_grid,cond,predict_param,extract_fit Prediction controls.
#'   \itemize{
#'     \item \code{n_grid} is an integer that defines the resolution of the surface (in both x and y directions).
#'     \item \code{cond} (optional) is a named list that defines the values of other predictors (i.e., those not in \code{view}) for which to make predictions. If un-supplied, factor variables are set at the most commonly occuring factor level and continuous variables are set at the closest observed value to the median.
#'     \item \code{predict_param} (optional) A named list of arguments, passed to \code{\link[stats]{predict}}, to customise predictions.
#'     \item \code{extract_fit} A function that extracts fitted values/values to be plotted from the object returned by \code{\link[stats]{predict}}.
#'   }
#' @param transform A function used to (back)-transform the predictor variables in \code{view} after prediction for plotting (i.e., the x and y axes). Use this option, for examples, if you scaled variables prior to model fitting and you want to back-transform them onto the natural scale.
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
#'                       predict_param = list(se.fit = TRUE),
#'                       extract_fit = function(p) p$se.fit)
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
#'
#' #### Example (9): Variable back-transformations
#' # see also pretty_predictions_1d()
#' # Define function to scale numbers without changing class
#' # (scale() creates a matrix)
#' scale_num <- function(x) {
#'   y <- scale(x)
#'   attributes(y)$dim <- NULL
#'   y
#' }
#' # Define function to unscale numbers, based on stored attributes
#' unscale <- function(x) {
#'   mu      <- attr(x, "scaled:center")
#'   sigma   <- attr(x, "scaled:scale")
#'   x * sigma + mu
#' }
#' # Scale predictors for modelling
#' x0s <- scale_num(x0)
#' x1s <- scale_num(x1)
#' x2s <- scale_num(x2)
#' gs    <- mgcv::gam(y ~ s(x0s, x1s, x2s))
#' # Plot predictions on original scale
#' pretty_predictions_2d(gs, view = c("x1s", "x2s"), transform = unscale)
#'
#' @seealso \code{\link[prettyGraphics]{pretty_predictions_1d}}
#' @author Edward Lavender
#' @export
#'

pretty_predictions_2d <- function(x, view = NULL,
                                  n_grid = 30, cond = list(), predict_param = list(), extract_fit = function(p) p,
                                  transform = NULL,
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
  terms <- model_terms(x)[-1L]
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
  preds <- extract_fit(preds)
  nd$pred <- preds
  z <- matrix(NA, n_grid, n_grid)
  s <- seq(0, nrow(nd), by = n_grid)
  for(i in seq_along(s)[-length(s)]) z[, i] <- nd$pred[(s[i] + 1):s[i+1]]

  #### Define axis limits
  if(!is.null(transform)) {
    attributes(xp) <- attributes(dat[, view[1]])
    attributes(yp) <- attributes(dat[, view[2]])
    xp <- transform(xp)
    yp <- transform(yp)
    dat[, view[1]] <- transform(dat[, view[1]])
    dat[, view[2]] <- transform(dat[, view[2]])
  }
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

  #### Add contours
  if(!is.null(add_contour)){
    add_contour$x <- xp
    add_contour$y <- yp
    add_contour$z <- z
    add_contour$add <- TRUE
    do.call(graphics::contour, add_contour)
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
