#' @title Create publication-quality plots
#' @description This function facilitates the creation of publication quality plots for timeseries. The function pulls lots of the functions provided in a complementary R package, \code{prettyGraphics}, which can be implemented sequentially, into a single framework. While it is often useful to call functions sequentially, this can make data exploration quicker and simpler. The function underlies \code{\link[prettyGraphics]{vis_ts}}.
#'
#' @param x The x coordinates.
#' @param y1 The y coordinates.
#' @param y2 (optional) The y coordinates of a second response variable.
#' @param fct (optional) A sequence of factor values.
#' @param fct_level (optional) A factor level for which to create the plot.
#' @param dat A dataframe containing columns named 'x', 'y1' (and optionally) 'y2' and 'fct' can be supplied instead of \code{x}, \code{y1} (and optionally) \code{y2} and \code{fct}.
#' @param pretty_axis_args A named list of arguments passed to \code{\link[prettyGraphics]{pretty_axis}} to create axes.
#' @param mtext_args A named list of arguments passed to \code{\link[graphics]{mtext}} to create axis labels.
#' @param add_points_args A named list of arguments passed to \code{\link[graphics]{points}} to add points.
#' @param add_lines_args A named list of arguments passed to \code{\link[prettyGraphics]{add_lines}} to add lines.
#' @param y2_method A character specifying how \code{y2} should be added to the plot. Implemented options are \code{"by_colour"} or \code{"by_new_axis"}.
#' @param insert_colour_bar A logical input defining whether or not to add a colour bar. This is useful if \code{y2_method = "by_colour"}.
#' @param add_colour_bar_args A named list of arguments passed to \code{\link[prettyGraphics]{add_colour_bar}}.
#' @param subplot_args A list of arguments passed to \code{\link[TeachingDemos]{subplot}} to adjust the location/size of the colour bar.
#' @param pretty_axis_args_y2 A named list of arguments passed to \code{\link[prettyGraphics]{pretty_axis}} to create a second y axis if \code{y2_method = "by_new_axis"}.
#' @param add_lines_args_y2 A named list of arguments passed to \code{\link[prettyGraphics]{add_lines}} to add \code{y2} as a line on a second axis.
#' @param list_CIs_args A named list of arguments passed to \code{\link[prettyGraphics]{list_CIs}} to add model predictions to a plot.
#' @param add_model_predictions_args A named list of arguments passed to \code{\link[prettyGraphics]{add_model_predictions}} to add model predictions.
#' @param summarise_in_bins_args A named list of arguments passed to \code{\link[prettyGraphics]{summarise_in_bins}} to compute summary statistics.
#' @param add_lines_args_summaries A named list of arguments passed to \code{\link[prettyGraphics]{add_lines}} to add summary lines to a plot.
#' @param add_shading_type A character input specifying the type of shading to be added. \code{"diel"} and \code{"season"} are supported. Custom shading can be added via supplying arguments to \code{add_shading_args} (see below).
#' @param add_shading_dtb_args A named list of arguments that are passed to \code{\link[prettyGraphics]{define_time_blocks}} to compute diel/seasonal shading. These include a named list of arguments passed to \code{type_args} and colours.
#' @param add_shading_args A named list of arguments passed to a\code{\link[prettyGraphics]{add_shading_bar}} to add shading to a plot. 'x1', 'x2', and 'lim' are computed automatically if \code{add_shading_type} is specified, but other graphical parameters passed to \code{\link[graphics]{rect}} (e.g. \code{border = "red"}) can be included here.
#' @param add_moons_args A named list of arguments passed to \code{\link[prettyGraphics]{add_moons}} to add moons to a plot.
#' @param return_list A logical input which defines whether or not to return the list of axis parameters computed by \code{\link[prettyGraphics]{pretty_axis}}. This can be useful for the addition of elements to a plot created by \code{pretty_ts()}.
#'
#' @examples
#'
#' #### Simulate some example date
#' set.seed(1)
#' x <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"),
#'                 as.POSIXct("2016-01-10", tz = "UTC"), by = "2 hours")
#' y1 <- rnorm(length(x), 200, 25) *-1
#' y2 <- rnorm(length(x), lubridate::yday(x) * 0.5 +20, 0.5)
#' fct <- sample(c(1, 2), length(x), replace = TRUE)
#' fct_level <- 1
#'
#' #### (1) The default options plot y1 against x
#' pp <- par(oma = c(2, 2, 2, 4))
#' pretty_ts(x = x, y1 = y1)
#'
#' ##### (2) A dataframe can be supplied instead with 'x' and 'y1' columns (and others, see below)
#' pretty_ts(dat = data.frame(x = x, y1 = y1))
#'
#' ##### (3) Plots can be created for different levels of a factor by providing 'fct' and 'fct_level'
#' # These can be provided via the arguments:
#' pretty_ts(x = x, y1 = y1, fct = fct, fct_level = fct_level)
#' # Or via dat, although fct_level should be specified separately:
#' pretty_ts(dat = data.frame(x = x, y1 = y1, fct = fct), fct_level = fct_level)
#'
#' #### (4) Axes can be adjusted by supplying arguments to pretty_axis() via pretty_axis_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE))
#'             )

#' #### (5) Axes labels can be supplied via arguments to mtext() via mtext_args
#' # each axes can be controlled separately via a nested list:
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3))
#'             )
#'
#' #### (6) Points can be added by supplying arguments to points() via add_points_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_points_args = list(pch = 21, col = "dimgrey", bg = "dimgrey", cex = 0.5)
#'             )
#'
#' #### (7) Lines are contolled by supplying arguments to add_lines() via add_lines_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(col = "red", lwd = 2))
#'
#' #### (8) To colour a line by a second variable, specify y2 and y2_method = "by_colour"
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2),
#'             y2_method = "by_colour")
#'
#' #### (9) We can supply further arguments to add_lines() via add_lines_args() to control colouration
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2, f = viridis::viridis),
#'             y2_method = "by_colour")
#'
#'
#' #### (10) If y2 is specified along with y2_method = "by_colour", a colour bar is automatically added
#' # ... This is because insert_colour_bar is TRUE by default.
#' # This can be turned off with insert_colour_bar = FALSE
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2, f = viridis::viridis),
#'             y2_method = "by_colour",
#'             insert_colour_bar = FALSE)
#'
#' #### (12) The axis of the colour bar can be adjusted by adjusting the call to add_lines_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2,
#'                                   f= viridis::inferno,
#'                                  pretty_axis_args = list(pretty = list(n = 5),
#'                                                           axis = list(las = TRUE))),
#'             y2_method = "by_colour",
#'             insert_colour_bar = TRUE
#'             )
#'
#' #### (13) Other adjustments (e.g. add a title) can made by passing arguments to add_colour_bar()
#' # ... via add_colour_bar_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2,
#'                                   f= viridis::inferno,
#'                                   pretty_axis_args = list(pretty = list(n = 5),
#'                                                           axis = list(las = TRUE))),
#'             y2_method = "by_colour",
#'             insert_colour_bar = TRUE,
#'             add_colour_bar_args = list(mtext_args = list(side = 4, text = "Colour Bar", line = 2))
#'             )
#'
#' #### (14) The size placement of the colour bar is controlled by passing arguments
#' # ... to TeachingDemos::subplot() via subplot_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2,
#'                                   f= viridis::inferno,
#'                                   pretty_axis_args = list(pretty = list(n = 5),
#'                                                           axis = list(las = TRUE))),
#'             y2_method = "by_colour",
#'             insert_colour_bar = TRUE,
#'             add_colour_bar_args = list(mtext_args = list(side = 4, text = "Colour Bar", line = 2)),
#'             subplot_args = list(y = -260, size = c(0.2, 2))
#'             )
#'
#' #### (15) Instead of via colouration, a second variable can be added using a new axis
#' # ... by specifying y2_method = "by_new_axis"
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2),
#'             y2_method = "by_new_axis"
#'             )
#'
#' #### (16) The colour of the second line is controlled by another call to add_lines() via
#' # ... add_lines_args_y2. This will add an axis to the fourth side by default:
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2),
#'             y2_method = "by_new_axis",
#'             add_lines_args_y2 = list(col = "red")
#'             )
#'
#' #### (17) The second axis can be controlled via another call to pretty_axis()
#' # ... via pretty_axis_args_y2 and we can update mtext_args to add a label:
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3),
#'                               list(side = 4, text = "Second Response", cex.axis = 1.5, line = 2)),
#'             add_lines_args = list(lwd = 2),
#'            y2_method = "by_new_axis",
#'             add_lines_args_y2 = list(col = "red"),
#'             pretty_axis_args_y2 = list(pretty = list(n = 5), axis = list(las = TRUE))
#'             )
#'
#' ##### (18) Model predictions can be added by supplying arguments to list_CIs() via list_CIs_args
#' # Define model and predictions
#' m1 <- lm(y1 ~ x)
#' p <- predict(m1, se.fit = TRUE)
#' # Make plot and add predictions
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2),
#'             y2_method = "by_new_axis",
#'             add_lines_args_y2 = list(col = "red"),
#'             pretty_axis_args_y2 = list(pretty = list(n = 5), axis = list(las = TRUE)),
#'             list_CIs_args = list(pred = p)
#'             )
#'
#' #### (19) The visualisation of model predictions can be controlled by supplying
#' # ... arguments to add_model_predictions() via add_model_predictions_args
#' pretty_ts(x = x,
#'             y1 = y1,
#'             y2 = y2,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             mtext_args = list(list(side = 1, text = "Time", cex.axis = 1.5, line = 2.5),
#'                               list(side = 2, text = "Response", cex.axis = 1.5, line = 3)),
#'             add_lines_args = list(lwd = 2),
#'             y2_method = "by_new_axis",
#'             add_lines_args_y2 = list(col = "red"),
#'             pretty_axis_args_y2 = list(pretty = list(n = 5), axis = list(las = TRUE)),
#'             list_CIs_args = list(pred = p),
#'             add_model_predictions_args = list(CI_gp = list(col = "skyblue"),
#'                                               fitted_gp = list(col = "blue"))
#'             )
#'
#'
#' #### (20) Statistical summarise of y1 in bins can be added
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             summarise_in_bins_args = list(bin = "days",
#'                                           funs = list(foo1 = mean,
#'                                                       foo2 = function(x){mean(x) + stats::sd(x)},
#'                                                       foo3 = function(x){mean(x) - stats::sd(x)}
#'                                                       )
#'                                           ),
#'             add_lines_args_summaries = list(col = "red", lwd = 2, lty = 3, type = "b")
#'             )
#'
#' #### (23) The graphical parameters of each summary statistic can be controlled using a nested list:
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             summarise_in_bins_args = list(bin = "days",
#'                                           funs = list(foo1 = mean,
#'                                                       foo2 = function(x){mean(x) + stats::sd(x)},
#'                                                       foo3 = function(x){mean(x) - stats::sd(x)}
#'                                           )
#'             ),
#'             add_lines_args_summaries = list(list(col = "red", lty = 2),
#'                                             list(col = "blue", lty = 3),
#'                                             list(col = "blue", lty = 3)
#'                                             )
#'             )
#'
#' #### (22) Shading can also be added, via add_shading_type, add_shading_dtb_args and/or
#' # ... add_shading_args. If add_shading_type = "diel" or "season", define_time_blocks()
#' # ...  is used to define x1, x2 and lim that are passed to add_shading_bar() internally.
#' # ... Otherwise, custom shading can be added.
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             add_shading_type = "diel",
#'             add_shading_dtb_args = list(col = c("white", "lightgrey"),
#'                                         type_args = list(lon = 65, lat = 4)
#'                                         )
#'            )
#'
#' #### (23) Shading can be adjusted by supplying additional arguments to add_shading_bar()
#' # ... (and, in turn, graphics::rect()) via add_shading_args:
#' pretty_ts(x = x,
#'             y1 = y1,
#'             pretty_axis_args = list(side = 1:2,
#'                                     pretty = list(n = 5),
#'                                     axis = list(las = TRUE)),
#'             add_shading_type = "diel",
#'             add_shading_dtb_args = list(col = c("white", "lightgrey"),
#'                                         type_args = list(lon = 65, lat = 4)
#'             ),
#'             add_shading_args = list(border = FALSE)
#' )
#' par(pp)
#'
#' @author Edward Lavender
#' @export
#'
#######################################
#######################################
#### plot_continuous

pretty_ts <-
  function(
    # Data
    x,
    y1,
    y2 = NULL,
    fct = NULL,
    fct_level = NULL,
    dat = NULL,
    # Axis arguments and names
    pretty_axis_args = list(pretty = list(n = 10)),
    mtext_args = list(),
    # Visualising data
    add_points_args = list(),
    add_lines_args = list(lwd = 1),
    y2_method = "by_colour", # by_second_axis
    insert_colour_bar = TRUE,
    add_colour_bar_args = list(),
    subplot_args = list(size = c(0.25, 2.5), vadj = 0, hadj = 0),
    pretty_axis_args_y2 = list(side = 4, pretty = list(n = 10)),
    add_lines_args_y2 = list(),
    # Model predictions
    list_CIs_args = list(),
    add_model_predictions_args = list(),
    # Statistical summaries and shading
    summarise_in_bins_args = list(),
    add_lines_args_summaries = list(lwd = 1),
    add_shading_type = NULL,
    add_shading_dtb_args = list(),
    add_shading_args = list(),
    # moons
    add_moons_args = list(),
    return_list = TRUE
  ){



  ################################################
  ################################################
  #### Define dataframe if not supplied

  #### Define dat
  if(is.null(dat)){
    dat <- data.frame(x = x, y1 = y1)
    if(!is.null(y2)){
      dat$y2 <- y2
    }
    if(!is.null(fct)){
      stopifnot(length(fct) == length(x))
      dat$fct <- fct
    }
  }

  if(!is.null(dat$fct) & !is.null(fct_level)){
     dat <- dat[which(dat$fct == fct_level), ]
    }

  # utils::head(dat)


  ################################################
  ################################################
  #### Use pretty_axis to define axes

  axis_ls <- implement_pretty_axis_args(list(dat$x, dat$y1), pretty_axis_args)

  '
  #### Define pretty axis args
  # pretty_axis_args = list(side = 1:2, pretty = list(n = 5))
  # merging with list(NULL) can cause issues, so we"ll first exlcude those arguments and, then, after merging,
  # ... if they"re absent we"ll add them back.
  dpa <-   list(side = 1:4,
                x = list(dat$x, dat$y1),
                # lim = list(NULL),
                pretty = list(n = 5),
                # units = list(NULL),
                # axis = list(NULL),
                axis_ls = NULL,
                add = FALSE,
                return_list = TRUE
                )
  pretty_axis_args <- rlist::list.merge(dpa, pretty_axis_args)
  list_add_list_NULL <- function(l, elm){
    for(i in elm){
      if(is.null(l[[i]])){
        l[[i]] <- list(NULL)
        }
    }
    return(l)
    }
  pretty_axis_args <- list_add_list_NULL(pretty_axis_args, c("lim", "units", "axis"))
  axis_ls <- do.call("pretty_axis", pretty_axis_args)
  '

  #### Extract x limits, which may pertain to side 1 or 3
  which_xlim <- which(c(!is.null(axis_ls$"1"$lim), !is.null(axis_ls$"3"$lim)))
  which_xlim <- which_xlim[1]
  which_xlim <- c("1", "3")[which_xlim]
  xlim <- axis_ls[[which_xlim]]$lim

  #### Extract y limits, which may pertain to side 2 or 4
  which_ylim <- which(c(!is.null(axis_ls$"2"$lim), !is.null(axis_ls$"4"$lim)))
  which_ylim <- which_ylim[1]
  which_ylim <- c("2", "4")[which_ylim]
  ylim <- axis_ls[[which_ylim]]$lim


  ################################################
  ################################################
  #### Plot a basic plot

  #### Plot, using limits from pretty_axis()
  graphics::plot(dat$x, dat$y1,
                 axes = FALSE,
                 xlab = "", ylab = "",
                 xlim = xlim, ylim = ylim,
                 type = "n")

  #### Clip (closed later)
  usr <- graphics::par("usr")
  graphics::clip(xlim[1], xlim[2], ylim[1], ylim[2])


  ################################################
  ################################################
  #### Add shading

  if(!is.null(add_shading_type) | length(add_shading_args > 0)){

    #### Compute data for adding shading
    if(add_shading_type %in% c("diel", "season")){
      add_shading_dat <-
        define_time_blocks(t1 = xlim[1],
                           t2 = xlim[2],
                           type = add_shading_type,
                           type_args = add_shading_dtb_args$type,
                           to_plot = TRUE,
                           col = add_shading_dtb_args$col
        )
      das <- list(x1 = add_shading_dat$x1,
                  x2 = add_shading_dat$x2,
                  col = add_shading_dat$col,
                  horiz = FALSE,
                  lim = ylim
                  )
      add_shading_args <- list_merge(das, add_shading_args)
    }

    #### add shading
    do.call("add_shading_bar", add_shading_args)
  }


  ################################################
  ################################################
  #### Add axes

  pretty_axis(axis_ls = axis_ls, add = TRUE)
  implement_mtext_args(mtext_args)

  '
  #### Add titles
  # mtext_args <- list(list(side = 1, "x", line = 3), list(side = 2, "y1", line = 3))
  if(length(mtext_args) > 0){
    lapply(mtext_args, function(mtext_args_side_i){
      do.call("mtext", mtext_args_side_i)
    })
  }
  '


  ################################################
  ################################################
  #### Add response as points/line

  #### Add points if specified
  if(length(add_points_args) > 0){
    dap <- list(x = dat$x, y = dat$y1)
    add_points_args <- list_merge(dap, add_points_args)
    do.call("points", add_points_args)
  }

  if(length(add_lines_args) > 0){
    #### Add a line for the repsonse
    dal <- list(x = dat$x,
                y1 = dat$y1,
                y2 = dat$y2,
                dat = NULL,
                pretty_axis_args = list(pretty = list(n = 5)),
                n = 100,
                f = grDevices::colorRampPalette(c("red", "blue")),
                output = 3)
    # If y2 is provided but the user has specified by_new_axis, we'll set this to NULL:
    if(y2_method == "by_new_axis"){
      dal$y2 <- NULL
    }
    add_lines_args <- rlist::list.merge(dal, add_lines_args)
    colour_line_ls <- do.call("add_lines", add_lines_args)
  }


  ################################################
  ################################################
  #### Add colour bar if necessary

  if(insert_colour_bar & !is.null(dat$y2) & y2_method == "by_colour"){
    dacb <- list(data_legend = colour_line_ls$data_legend,
                 pretty_axis_args = colour_line_ls$axis_legend,
                 mtext_args = list(),
                 data_raw = NULL,
                 mark_args = list()
                 )
    add_colour_bar_args <- list_merge(dacb, add_colour_bar_args)
    dsp <- list(x = xlim[2], y = ylim[1], size = c(0.25, 2), vadj = 0, hadj = 0)
    subplot_args <- list_merge(dsp, subplot_args)
    TeachingDemos::subplot(x = subplot_args$x,
                           y = subplot_args$y,
                           size = subplot_args$size,
                           vadj = subplot_args$vadj,
                           hadj = subplot_args$hadj,
                           fun = do.call("add_colour_bar", add_colour_bar_args)
                           )

    }


  ################################################
  ################################################
  #### Add lines for summary statistics

  # summarise_in_bins_args <- list(bin = "hours")
  # add_lines_args_summaries <- list(col = "red")
  if(length(summarise_in_bins_args) > 0){
    dsin <- list(x = dat$x,
                 y = dat$y1,
                 bin = 10,
                 breaks = NULL,
                 funs = list(),
                 shift = TRUE,
                 to_plot = TRUE,
                 output = "list")
    summarise_in_bins_args <- rlist::list.merge(dsin, summarise_in_bins_args)
    summary_ls <- do.call("summarise_in_bins", summarise_in_bins_args)
    if(plotrix::listDepth(add_lines_args_summaries) == 1){
      add_lines_summaries_args_ls <- lapply(1:length(summary_ls), function(a){ add_lines_args_summaries })
    } else{
      add_lines_summaries_args_ls <- add_lines_args_summaries
    }
    mapply(summary_ls,
           add_lines_summaries_args_ls,
           FUN = function(summary_df,
                          add_lines_summaries_args_foo){
      # add_lines_summaries_args_foo <- add_lines_summaries_args_ls[[1]]
      add_lines_summaries_args_foo <- list_merge(list(x = summary_df$bin, y1 = summary_df$stat), add_lines_summaries_args_foo)
      do.call("add_lines", add_lines_summaries_args_foo)
    })
  }


  ################################################
  ################################################
  #### Add model predictions

  if(length(list_CIs_args) > 0){
    #### Define CIs
    dlCIs <- list(inv_link = I,
                  fadj = I,
                  centre = FALSE,
                  plot_suggestions = FALSE,
                  pretty_param = list()
    )
    list_CIs_args <- rlist::list.merge(dlCIs, list_CIs_args)
    CIs <- do.call("list_CIs", list_CIs_args)

    #### Add model predictions
    damp <- list(x = dat$x,
                 CI = CIs,
                 fCI = "poly",
                 CI_gp = list(col = "lightgrey", border = FALSE),
                 add_fitted = TRUE,
                 fitted_gp = list(col = "black", lwd = 1, lty = 1)
    )
    add_model_predictions_args <- list_merge(damp, add_model_predictions_args)
    # Delete the default border = FALSE option if fCI = "lines" because
    # ... this is not an argument to lines:
    if(add_model_predictions_args$fCI == "lines"){
      add_model_predictions_args$CI_gp$border <- NULL
    }
  }

  # Implement do.call("add_model_predictions", add_model_predictions_args) outside
  # ... of if(length(list_CIs_args) > 0) because the user may supply a suitable list
  # ... e.g. created by simulate_posterior_obs() without going via list_CIs()
  if(length(list_CIs_args) > 0 | length(add_model_predictions_args) > 0){
    damp <- list(fCI = "poly",
                 CI_gp = list(col = "lightgrey", border = FALSE),
                 add_fitted = TRUE,
                 fitted_gp = list(col = "black", lwd = 1, lty = 1))
    add_model_predictions_args <- rlist::list.merge(damp, add_model_predictions_args)
    do.call("add_model_predictions", add_model_predictions_args)
  }



  ################################################
  ################################################
  #### Add moons

  if(length(add_moons_args) > 0){
    dam <- list(outer = TRUE, nv = 100, radius1 = 0.1, units = "radians")
    if(add_moons_args$side == 1){
      dam$position <- ylim[1]
    } else if(add_moons_args$side == 2){
      dam$position <- xlim[1]
    } else if(add_moons_args$side == 3){
      dam$position <- ylim[2]
    } else if(add_moons_args$side == 4){
      dam$position <- xlim[2]
    } else{
      warning("add_moons_args$side unsupported and add_moons() not implemented.")
    }
    add_moons_args <- rlist::list.merge(dam, add_moons_args)
    do.call(add_moons, add_moons_args)
  }

  ################################################
  ################################################
  #### Add line for y2

  #### close clip
  do.call("clip", as.list(usr))

  #### If y2 is provided and the user wants to add this as a separate line...
  if(y2_method == "by_new_axis"){

    #### Define axis parameters
    dpa_y2 <- list(side = 4,
                   x = list(dat$y2),
                   # lim = list(NULL),
                   pretty = list(n = 5),
                   # units = list(NULL),
                   # axis = list(NULL),
                   # axis_ls = NULL,
                   add = FALSE,
                   return_list = TRUE
    )
    pretty_axis_args_y2 <- rlist::list.merge(dpa_y2, pretty_axis_args_y2)
    list_add_list_NULL <- function(l, elm){
      for(i in elm){
        if(is.null(l[[i]])){
          l[[i]] <- list(NULL)
        }
      }
      return(l)
    }
    pretty_axis_args_y2 <- list_add_list_NULL(pretty_axis_args_y2, c("lim", "units", "axis"))
    axis_ls_y2 <- do.call("pretty_axis", pretty_axis_args_y2)
    if(axis_ls_y2[[1]]$axis$side == 1){
      axis_ls_y2[[1]]$axis$pos <- ylim[1]
    } else if(axis_ls_y2[[1]]$axis$side == 2){
      axis_ls_y2[[1]]$axis$pos <- xlim[1]
    } else if(axis_ls_y2[[1]]$axis$side == 3){
      axis_ls_y2[[1]]$axis$pos <- ylim[1]
    } else if(axis_ls_y2[[1]]$axis$side == 4){
      axis_ls_y2[[1]]$axis$pos <- xlim[2]
    }

    #### Obtain y limits
    y2lim <- axis_ls_y2[[1]]$lim

    #### New blank plot
    pp <- graphics::par(new = T)
    graphics::plot(dat$x, dat$y2,
                   xlim = xlim,
                   ylim = y2lim,
                   axes = F,
                   xlab = "", ylab = "",
                   type = "n")

    #### Add new axis to existing plot
    pretty_axis(axis_ls = axis_ls_y2, add = TRUE)

    #### Clip
    usr <- graphics::par("usr")
    graphics::clip(xlim[1], xlim[2], y2lim[1], y2lim[2])

    #### Add as a line
    daly2 <- list(x = dat$x, y1 = dat$y2)
    add_lines_args_y2 <- list_merge(daly2, add_lines_args_y2)
    do.call("add_lines", add_lines_args_y2)

    #### Restore clip
    do.call("clip", as.list(usr))

    #### Restore par
    graphics::par(pp)
  }

  #### Return list of outputs
  if(return_list){
    return(axis_ls)
  }

  #### close function
  }



#### End of code.
################################################
################################################
