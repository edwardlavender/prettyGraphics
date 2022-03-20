#######################################
#######################################
#### add_north_arrow()

#' @title Add a North arrow to a map
#' @description This function adds a North arrow to a map.
#'
#' @param x0,y0 A character keyword or doubles that define the arrow's position (see \code{\link[grDevices]{xy.coords}}).
#' @param width,mid,height Arrow shape controls. Each argument accepts a double that defines the arrow \code{width}, \code{mid}-point or \code{height}. \code{mid} is only used for \code{type = "filled head"}.
#' @param type A character string that defines the arrow type. Currently supported options are (a) \code{"filled head"}, which adds a (filled) arrow head (via \code{\link[graphics]{polygon}}); (b) \code{"filled triangle"}, which adds a (filled) triangle (via \code{\link[graphics]{polygon}}); and (c) \code{"arrow"}, which adds an arrow (via \code{\link[graphics]{arrows}}).
#' @param add_label A named list, passed to  \code{\link[graphics]{text}}, to add a label to the plot. \code{add_label = NULL} suppresses this option, \code{add_label = list()} implements default options and a named list customises these.
#' @param col,... Additional graphical parameters passed to \code{\link[graphics]{polygon}} or \code{\link[graphics]{arrows}}.
#'
#' @details Future functionality should include the capacity to control the tilt of the arrow (which is currently vertical) and to add north and east arrows that act as scale bars.
#'
#' @return The function adds an arrow to a map and invisibly returns a named list with the \code{x} and \code{y} coordinates of the arrow (derived from \code{x0}, \code{y0}, \code{width}, \code{mid} and \code{height} as applicable).
#'
#' @examples
#' #### Example (1): Implement function using the default options
#' raster::plot(dat_gebco)
#' add_north_arrow()
#'
#' #### Example (2): Adjust arrow location
#' raster::plot(dat_gebco)
#' add_north_arrow(x0 = "topright")
#' raster::plot(dat_gebco)
#' add_north_arrow(x0 = -5.55, y = 56.44)
#'
#' #### Example (3): Adjust arrow type
#' raster::plot(dat_gebco)
#' add_north_arrow("topleft")
#' add_north_arrow("topright", type = "filled triangle")
#' add_north_arrow("center", type = "filled triangle")
#' add_north_arrow("right", type = "arrow")
#'
#' #### Example (4): Adjust arrow type parameters
#' raster::plot(dat_gebco)
#' add_north_arrow("topleft", type = "filled triangle", border = "red")
#' add_north_arrow("center", type = "filled triangle", col = "blue")
#' add_north_arrow("top", type = "arrow", length = 0.05, lwd = 2)
#'
#' #### Example (5): Add arrow label
#' raster::plot(dat_gebco)
#' add_north_arrow(add_label = list())
#'
#' @author Edward Lavender
#' @export

add_north_arrow <- function(x0 = "topleft", y0 = NULL,
                            width, mid, height,
                            type = c("filled head", "filled triangle", "arrow"),
                            add_label = NULL,
                            col = "black",...){
  #### Define type
  type <- match.arg(type)

  #### Define anchor position
  if(inherits(x0, "character")){
    if(!is.null(y0)) warning("Argument 'y0' ignored.", call. = FALSE)
    position <- graphics::legend(x = x0, legend = "", bty = "n")
    x0 <- position$text$x
    y0 <- position$text$y
  } else {
    if(is.null(y0)) stop("'y0' must be specified.", call. = FALSE)
  }
  boundaries <- graphics::par("usr")

  ##### Use distances for height/width
  #
  # To be developed.
  #

  #### Define type-specific parameters
  ## Width and height for triangle type
  if(missing(width)){
    width <- (boundaries[2] - boundaries[1])/15
  }
  if(missing(height)){
    height <- (boundaries[4] - boundaries[3])/10
  }
  if(type == "filled head"){
    if(missing(mid)){
      mid <- height * 3/4
    } else warning("Argument 'mid' only used with type = 'filled head'.", call. = FALSE)
  }

  #### Add arrow to map
  ## polygon arrows
  if(grepl("filled", type, fixed = TRUE)){
    if(type == "filled triangle"){
      x <- c(x0, x0 - width/2, x0 + width/2)
      y <- c(y0, y0 - height, y0 - height)
    } else if(type == "filled head"){
      x <- c(x0, x0 - width/2, x0, x0 + width/2)
      y <- c(y0, y0 - height, y0 - mid, y0 - height)
    }
    graphics::polygon(x, y, col = col,...)
  } else {
    ## arrows arrows
    if(type == "arrow"){
      x <- x0
      y <- y0
      graphics::arrows(x0 = x0, y0 = y0 - height, x1 = x0, y1 = y0,...)
    }
  }

  #### Add label
  if(!is.null(add_label)){
    check_named_list(arg = NULL, l = add_label)
    if(is.null(add_label$labels)) add_label$labels <- "N"
    if(is.null(add_label$x)) add_label$x <- x0
    if(is.null(add_label$y)) add_label$y <- y0 + y0 * 0.0001
    do.call(graphics::text, add_label)
  }

  #### Return outputs
  return(invisible(list(x, y)))
}


#######################################
#######################################
#### summarise_by_lat()

#' @title Summarise a \code{\link[raster]{raster}} by latitude
#' @description This function summarises the values of a \code{\link[raster]{raster}} across latitudinal bands using a user-supplied function, such as \code{\link[base]{mean}}.
#' @param x A \code{\link[raster]{raster}}.
#' @param stat A function that summarises the values of \code{x} across latitudinal bands.
#' @details This routine is implemented within \code{\link[prettyGraphics]{add_profile_lat}}.
#' @return The function returns a dataframe with the latitudinal coordinates of each band (`y') and the summary (`stat'),
#' @examples
#' d <- summarise_by_lat(dat_gebco)
#' utils::head(d)
#' @author Edward Lavender
#' @export
#'

summarise_by_lat <- function(x, stat = mean){
  x_df <- data.frame(raster::rasterToPoints(x))
  colnames(x_df) <- c("x", "y", "z")
  x_tbl <- tapply(x_df$z, x_df$y, stat)
  x_df <- data.frame(y = as.numeric(names(x_tbl)), stat = as.numeric(x_tbl))
  x_df <- x_df[order(x_df$y), ]
  # dplyr solution (required rlang .data pronoun)
  # x %>%
  #  raster::rasterToPoints() %>%
  #  data.frame() %>%
  #  dplyr::rename("x" = 1, "y" = 2, "z" = 3) %>%
  #  dplyr::group_by(.data$y) %>%
  #  dplyr::summarise(stat = stat(.data$z), .groups = "drop_last") %>%
  #  data.frame()
  return(x_df)
}


#######################################
#######################################
#### add_profile_lat()

#' @title Add a latitudinal profile to a \code{\link[raster]{raster}} plot
#' @description This function is designed to add summary latitudinal profiles alongside a plot of a \code{\link[raster]{raster}}. To implement the function, the plotted \code{\link[raster]{raster}} must be supplied (\code{x}) as well as a summary statistic, such as \code{\link[base]{mean}}, via \code{calc_fit} that is used to summarise \code{\link[raster]{raster}} values across latitudinal bands (via \code{\link[prettyGraphics]{summarise_by_lat}}). `Lower' and `upper' summaries, such as the 25th and 75th percentiles can also be calculated, if desired, via \code{calc_lwr} and \code{calc_upr}. A plot with these summaries (represented by a `fitted' line and surrounding variability envelope, if applicable) is then added to the current plot. The properties of the added plot are controlled via standard \code{\link[prettyGraphics]{prettyGraphics}}'s arguments, such as \code{pretty_axis_args}. The graphical properties of the `fitted' line and surrounding envelope are controlled with named lists of arguments, via \code{add_fit} and \code{add_ci}. Placement is controlled by specifying the x and y locations on the current plot at which to add the new plot, via \code{x_at} and \code{y_at}.
#'
#' @param x A \code{\link[raster]{raster}}.
#' @param calc_fit A function that summarises the values of \code{x} across latitudinal bands, returning a single number for each band (see \code{\link[prettyGraphics]{summarise_by_lat}}).
#' @param calc_lwr,calc_upr (optional) Functions that calculate `lower' and `upper' summaries of the values of \code{x} across latitudinal bands.
#' @param xlim,ylim,pretty_axis_args,...,axes Axis control arguments for the added profile. \code{xlim} and \code{ylim} control axis limits. If un-supplied, \code{ylim} is defined to be the latitudinal range of \code{x}. \code{pretty_axis_args} is a named list of arguments, passed to \code{\link[prettyGraphics]{pretty_axis}} for finer control. Arguments to the \code{control_axis} argument of \code{\link[prettyGraphics]{pretty_axis}} can also be passed via \code{...}. Under the default options, an x axis is added at the `top' of the plot while the y axis is not shown. \code{axes} is a logical input that defines whether or not to add axes at all (\code{axes = FALSE} suppresses these). However, note that may still be necessary to specify \code{ylim} (and \code{y_at}) such that that the profile is added correctly alongside the plotted \code{\link[raster]{raster}}.
#' @param x_at,y_at Numeric vectors of length two that specify the x and y positions, on the current plot, for the four corners of the added plot. If un-supplied, the two \code{x_at} coordinates are taken as the maximum x limit of \code{x} and the same value plus 25 percent of the difference between the maximum and minimum x limits. \code{y_at} is taken from \code{ylim} which, if un-supplied, is defined by the y limits of \code{x}.
#' @param add_fit,add_ci Named lists of arguments, passed to \code{\link[prettyGraphics]{add_lines}} and \code{\link[graphics]{polygon}} to customise the appearance of the `fitted' line and, if necessary, the surrounding variability envelope defined by \code{calc_lwr} and \code{calc_upr}. For the fitted line, the \code{y2} argument is automatically supplied to \code{\link[prettyGraphics]{add_lines}} so that the line can follow the same colour scheme as the \code{\link[raster]{raster}}, if supplied (see Examples).
#'
#' @examples
#' #### Example (1): Implement the function using default options
#' pretty_map(dat_gebco, add_rasters = list(x = dat_gebco))
#' add_profile_lat(x = dat_gebco)
#'
#' #### Example (2): Control the properties of the 'fitted' line and/or envelope
#' ## E.g. Colour the 'fitted' line following the raster's colour scheme
#' zlim <- raster::cellStats(dat_gebco, "range")
#' col_param <- pretty_cols_split_heat(zlim)
#' pretty_map(dat_gebco,
#'            add_rasters = list(x = dat_gebco, zlim = zlim,
#'                               breaks = col_param$breaks, col = col_param$col))
#' add_profile_lat(x = dat_gebco,
#'                 add_fit = list(breaks = col_param$breaks, cols = col_param$col, lwd = 3))
#'
#' #### Example (2) Control axes
#' # E.g. Control via pretty_axis_args
#' pretty_map(dat_gebco, add_rasters = list(x = dat_gebco))
#' add_profile_lat(x = dat_gebco,
#'                 pretty_axis_args = list(side = 3:4))
#' # E.g. Suppress axes
#' pretty_map(dat_gebco, add_rasters = list(x = dat_gebco))
#' add_profile_lat(x = dat_gebco, axes = FALSE)
#'
#' #### Example (3) Control profile placement
#' axis_ls <- pretty_map(dat_gebco, add_rasters = list(x = dat_gebco))
#' add_profile_lat(x = dat_gebco,
#'                 x_at = c(axis_ls[[1]]$lim[2], axis_ls[[1]]$lim[2] + 0.05),
#'                 y_at = axis_ls[[2]]$lim)
#'
#'
#' @return The function adds a latitudinal summary profile to an existing plot of a \code{\link[raster]{raster}}.
#' @author Edward Lavender
#' @export
#'

add_profile_lat <- function(x,
                            calc_fit = mean,
                            calc_lwr = function(x) stats::quantile(x, 0.25),
                            calc_upr = function(x) stats::quantile(x, 0.75),
                            xlim = NULL, ylim = NULL,
                            pretty_axis_args = list(side = 3:2,
                                                    axis = list(list(),
                                                                list(lwd = 0, labels = FALSE))),
                            axes = TRUE,
                            x_at = NULL, y_at = ylim,
                            add_fit = list(),
                            add_ci = list(col = scales::alpha("lightgrey", 0.8), border = FALSE),...
                            ){

  #### Calculate 'fitted' summary by latitude
  ext <- raster::extent(x)
  fit_by_lat <- summarise_by_lat(x, stat = calc_fit)

  #### Define plot limits and axes
  ## Set ylim as the latitudinal range in x, unless supplied
  if(is.null(ylim)) {
    y_rng <- ext[3:4]
    if(is.null(pretty_axis_args$lim)) {
      ylim <- y_rng
    } else {
      if(is.null(pretty_axis_args$lim[[2]])) ylim <- y_rng
    }
  }
  ## Define pretty axes and pull out limits
  axis_ls <- implement_pretty_axis_args(x = list(x = fit_by_lat$stat, y = fit_by_lat$y),
                                        pretty_axis_args = pretty_axis_args,
                                        xlim = NULL,
                                        ylim = ylim,...)
  xlim <- axis_ls[[1]]$lim
  ylim <- axis_ls[[2]]$lim

  #### Define plot placement
  if(is.null(x_at)){
    x_at <- rep(ext[2], 2)
    x_at[2] <- x_at[2] + 0.25 * abs(ext[1] - ext[2])
  }
  if(is.null(y_at)) y_at <- ylim

  #### Write function to add summary profile to plot
  add_profile <- function(){

    ## Create a blank plot
    plot(fit_by_lat$stat, fit_by_lat$y,
         xlab = "", ylab = "",
         xlim = xlim, ylim = ylim,
         type = "n",
         axes = FALSE)
    ## Add variability interval, if applicable
    if(!is.null(calc_lwr) & !is.null(calc_upr)){
      lwr_by_lat <- summarise_by_lat(x, stat = calc_lwr)
      upr_by_lat <- summarise_by_lat(x, stat = calc_upr)
      add_ci$x <- as.numeric(c(lwr_by_lat$stat, rev(upr_by_lat$stat)))
      add_ci$y <- as.numeric(c(fit_by_lat$y, rev(fit_by_lat$y)))
      do.call(graphics::polygon, add_ci)
    }
    ## Add fitted values
    # Define colour bar limits as ylim
    # Provide 'y2' to colour bar so the line can be coloured by the raster colour scheme
    add_fit$x <- fit_by_lat$stat
    add_fit$y1 <- fit_by_lat$y
    add_fit$y2 <- fit_by_lat$stat
    add_fit$pretty_axis_args = list(lim = list(ylim))
    do.call(add_lines, add_fit)
    # Add axes
    if(axes) pretty_axis(axis_ls = axis_ls, add = TRUE)
  }

  #### Add latitudinal profile
  pp <- graphics::par(xaxs = "i", yaxs = "i")
  on.exit(graphics::par(pp))
  TeachingDemos::subplot(fun = add_profile(),
                          x = x_at,
                          y = y_at)
  return(invisible())
}
