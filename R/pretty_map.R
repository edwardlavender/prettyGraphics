#######################################
#######################################
#### add_sp_*()

#' @title Add spatial layers to a background map
#' @description These functions are designed to facilitate the addition of spatial layers to a background map.
#'
#' @param x A Raster* or Spatial* object, a two-column matrix of x and y coordinates or a numeric vector of x coordinates. Coordinate specifications are only supported for points, lines and paths; otherwise a Raster* or Spatial* object should be supplied. A numeric vector for \code{x} is assumed if \code{y} is provided (see below).
#' @param y (optional) A numeric vector of y coordinates. This is only required if \code{x} is a numeric vector of x coordinates.
#' @param ext (optional) An \code{\link[raster]{extent}} object that defines the extent of an area. If \code{crop_spatial = TRUE}, then the object is cropped to lie within this area via \code{\link[raster]{crop}} (see below).
#' @param crop_spatial (optional) A logical variable that defines whether or not to crop the spatial layer to lie within the domain defined by \code{ext}. This is only implemented if \code{ext} is provided.
#' @param ... Additional arguments passed to the plotting functions, which are \code{\link[fields]{image.plot}} for rasters, \code{\link[graphics]{points}} for points and \code{\link[raster]{plot}} for all other objects.
#'
#' @details These functions are designed to work with \code{\link[prettyGraphics]{pretty_map}}, which produces a background plot and then adds layers to this plot. However, they can also be called directly after the definition of a background plot.
#' @return The function adds a spatial layers to an existing plot.
#'
#' @examples
#' #### Example (1) Adding single layers
#'
#' ## Examples with raster
#' # Define a background map and add a raster layer
#' pretty_map(dat_gebco)
#' add_sp_raster(dat_gebco)
#' # Customise the map
#' add_sp_raster(dat_gebco, col = viridis::viridis(100))
#'
#' ## Examples with polygon layers
#' # Define a background map and add a polygon layer
#' pretty_map(dat_coast_around_oban)
#' add_sp_poly(dat_coast_around_oban)
#' # Customise the map
#' add_sp_poly(dat_coast_around_oban, col = "darkgreen")
#'
#' ## Examples with points, lines and paths
#' ## Define coordinates/line
#' xy <- cbind(c( -5.532913, -5.519556, -5.500856, -5.472138),
#'             c(56.42964, 56.44220, 56.45328, 56.45846))
#' xy_line <- Orcs::coords2Lines(xy, ID = 1)
#' ## Add points
#' # via coordinates
#' pretty_map(dat_gebco)
#' add_sp_points(x = xy[1, ], y = xy[2, ])
#' # via matrix
#' pretty_map(dat_gebco)
#' add_sp_points(x = xy)
#' # via SpatialPoints
#' pretty_map(dat_gebco)
#' add_sp_points(x = sp::SpatialPoints(xy))
#' # Add lines (best via SpatialLines object)
#' pretty_map(dat_gebco)
#' add_sp_line(x = xy_line)
#' # Add path via coordinates, SpatialPoints or SpatialLines
#' pretty_map(dat_gebco)
#' add_sp_path(x = xy[, 1], y = xy[, 2], length = 0.05)
#' pretty_map(dat_gebco)
#' add_sp_path(x = sp::SpatialPoints(xy), length = 0.05)
#' pretty_map(dat_gebco)
#' add_sp_path(x = xy_line, length = 0.05)
#'
#' #### Example (2) Layers can be stacked
#' pretty_map(dat_gebco)
#' add_sp_raster(dat_gebco)
#' add_sp_poly(dat_coast_around_oban)
#'
#' #### Example (3) These functions are implemented pretty_map()
#' ## Map of raster
#' pretty_map(add_raster = list(x = dat_gebco))
#' ## Map with a raster and multiple polygons, supplied as nested list
#' # Generate a random prism to include as a polygon
#' sim_prism <- sp::spsample(dat_coast_around_oban, n = 3, type = "random")
#' sim_prism <- sp::Polygon(sim_prism)
#' sim_prism <- sp::SpatialPolygons(list(sp::Polygons(list(sim_prism), ID = 1)))
#' # Make map
#' pretty_map(add_rasters = list(x = dat_gebco),
#'            add_polys = list(list(x = dat_coast_around_oban, col = "darkgreen"),
#'                             list(x = sim_prism, col = "blue")))
#'
#' @author Edward Lavender
#' @name add_sp
NULL


#### add_sp_raster()
#' @rdname add_sp
#' @export

add_sp_raster <- function(x, ext = NULL, crop_spatial = FALSE,...){
  # Crop raster
  if(!is.null(ext) & crop_spatial) x <- raster::crop(x, ext)
  # Gather parameters
  param <- list(x = x,...)
  # Define zlim across range of data
  if(is.null(param$zlim)) {
    rng <- c(raster::minValue(x), raster::maxValue(x))
    zlim <- rng
    # zlim <- range(pretty_seq(rng, pretty_args = list(n = 2)))
    param$zlim <- zlim
  }
  # Use default colouration implemented by raster::plot() rather than fields::image.plot()
  if(is.null(param$col)) param$col <- rev(grDevices::terrain.colors(255))
  # Add spatial surface
  param$add <- TRUE
  do.call(fields::image.plot, param)
  return(invisible())
}


#### add_sp_poly()
#' @rdname add_sp
#' @export

add_sp_poly <- function(x, ext = NULL, crop_spatial = FALSE,...){
  if(!is.null(ext) & crop_spatial) x <- raster::crop(x, ext)
  param <- list(x = x,...)
  param$add <- TRUE
  do.call(raster::plot, param)
  return(invisible())
}


#### add_sp_line()
#' @rdname add_sp
#' @export

add_sp_line <- function(x, y = NULL, ext = NULL, crop_spatial = FALSE,...){
  if(!is.null(y)) x <- cbind(x, y)
  if(inherits(x, "matrix")) x <- sp::SpatialPoints(x)
  if(!is.null(ext) & crop_spatial) x <- raster::crop(x, ext)
  param <- list(x = x,...)
  param$add <- TRUE
  do.call(raster::plot, param)
  return(invisible())
}


#### add_sp_path()
#' @rdname add_sp
#' @export

add_sp_path <- function(x, y = NULL, ext = NULL, crop_spatial = FALSE,...){
  if(!is.null(y)) x <- cbind(x, y)
  if(inherits(x, "matrix")) x <- sp::SpatialPoints(x)
  if(!is.null(ext) & crop_spatial) x <- raster::crop(x, ext)
  x <- sp::coordinates(x)
  if(inherits(x, "list")) x <- purrr::flatten(x)[[1]]
  s <- 1:(nrow(x) - 1)
  param <- list(...)
  param$x0 <- x[s, 1]
  param$x1 <- x[s + 1, 1]
  param$y0 <- x[s, 2]
  param$y1 <- x[s + 1, 2]
  do.call(graphics::arrows, param)
  return(invisible())
}

#### add_sp_points()
#' @rdname add_sp
#' @export

add_sp_points <- function(x, y = NULL, ext = NULL, crop_spatial = FALSE,...){
  if(!is.null(y)) x <- cbind(x, y)
  if(inherits(x, "matrix")) x <- sp::SpatialPoints(x)
  if(!is.null(ext) & crop_spatial) x <- raster::crop(x, ext)
  param <- list(x,...)
  do.call(graphics::points, param)
  return(invisible())
}


#######################################
#######################################
#### pretty_map()

#' @title Pretty maps
#' @description This function is used to produce pretty maps. This function proceeds by plotting a background map with pretty axes and then adds specifies spatial layers (namely, rasters, polygons, lines, paths and points) to this plot. Appropriate axis limits can be derived across all inputted spatial objects (unless specified) and all spatial layers can be cropped to this area (if requested).
#'
#' @param x (optional) An \code{\link[raster]{extent}} object (or, preferably, an object, such as a Raster* or Spatial* object that includes a coordinate reference system (CRS) and from which an \code{\link[raster]{extent}} object can be derived) that defines the area's boundaries. If provided, this is used to define the background plot, including the CRS (if possible) and axis limits (if unspecified). If \code{x = NULL}, then the CRS and axis limits are derived from the spatial objects defined via \code{add_*} lists (see below) and, if applicable, other function arguments, such as \code{xlim} and \code{ylim}.
#' @param add_rasters (optional) A (optionally nested) named list of arguments, passed to \code{\link[prettyGraphics]{add_sp_raster}}, to add raster(s) to the plot. Nested lists are supported for the addition of multiple layers. Each list must contain an 'x' element that defines the raster to be added to the plot.
#' @param add_polys (optional) A (optionally nested) named list of arguments, passed to \code{\link[prettyGraphics]{add_sp_poly}}, to add polgyon(s) to the plot. The implementation of this argument follows that of \code{add_rasters} (above).
#' @param add_lines (optional) A (optionally nested) named list of arguments, passed to \code{\link[prettyGraphics]{add_sp_line}}, to add lines(s) to the plot. The implementation of this argument follows that of \code{add_rasters} (above), but x and y coordinates can be passed as vectors, a matrix or as a Spatial* object.
#' @param add_paths (optional) A (optionally nested) named list of arguments, passed to \code{\link[prettyGraphics]{add_sp_path}}, to add path(s) to the plot. The implementation of this argument follows that of \code{add_rasters} (above), but x and y coordinates can be passed as vectors, a matrix or as a Spatial* object.
#' @param add_points (optional)  A (optionally nested) named list of arguments, passed to \code{\link[prettyGraphics]{add_sp_points}}, to add points(s) to the plot. The implementation of this argument follows that of \code{add_rasters} (above), but x and y coordinates can be passed as vectors, a matrix or as a Spatial* object.
#' @param add_additional (optional) A stand-alone function, to be executed after the background plot has been made and any specified spatial layers have been added to this, to customise the result.
#' @param crop_spatial A logical variable that defines whether or not to crop spatial data to lie within the axis limits, which are defined from (a) \code{x}, \code{xlim} and \code{ylim}, or from inputted spatial objects, depending on user inputs.
#' @param xlim,ylim,pretty_axis_args Axis control arguments. \code{xlim} and \code{ylim} control the axis limits, following the rules of the 'lim' argument in \code{\link[prettyGraphics]{pretty_axis}}. Finer control can be achieved by passing additional arguments to this function as a named list via \code{pretty_axis_args}.
#' @param verbose A logical variable that defines whether or not to print messages to the console to relay function progress. This can be useful with very large spatial datasets.
#' @param ... Additional arguments, passed to \code{\link[raster]{plot}}, which creates the background plot, such as \code{xlab}, \code{ylab} and \code{main}.
#'
#' @return The function produces a background plot of an area with spatial layers added (if applicable).
#'
#' @examples
#' #### Example (1): Background only plots
#' pretty_map(dat_gebco)
#' pretty_map(dat_coast_around_oban)
#' pretty_map(raster::extent(-10, 10, -10, 10))
#'
#' #### Example (2): Single spatial layers
#' # Plot a bathymetric map
#' pretty_map(dat_gebco, add_raster = list(x = dat_gebco))
#' # With a single spatial layer, this is an equivalent implementation
#' pretty_map(add_raster = list(x = dat_gebco))
#' # With multiple layers, we can get the extent of the area automatically
#' x <- runif(1000, -6, -4)
#' y <- runif(1000, 55, 58)
#' pretty_map(add_rasters = list(x = dat_gebco),
#'            add_points = list(x = x, y = y))
#' # Or we can set it by specifying 'x' and, if necessary, crop other spatial data
#' # ... to this area
#' pretty_map(x = dat_gebco,
#'            add_rasters = list(x = dat_gebco),
#'            add_points = list(x = x, y = y),
#'            crop_spatial = TRUE)
#' # Or via xlim and ylim arguments
#' pretty_map(x = dat_gebco,
#'            add_rasters = list(x = dat_gebco),
#'            add_points = list(x = x, y = y),
#'            xlim = c(-5.5, -5.45), ylim = c(56.4, 56.45),
#'            crop_spatial = TRUE)
#'
#' #### Example (3): Use nested lists to inlclude multiple elements of the same type
#' ## E.g., A map with a raster and multiple polygons, supplied as nested list
#' # Generate a random prism to include as a polygon
#' sim_prism <- sp::spsample(dat_coast_around_oban, n = 3, type = "random")
#' sim_prism <- sp::Polygon(sim_prism)
#' sim_prism <- sp::SpatialPolygons(list(sp::Polygons(list(sim_prism), ID = 1)))
#' # Make map
#' pretty_map(add_rasters = list(x = dat_gebco),
#'            add_polys = list(list(x = dat_coast_around_oban, col = "darkgreen"),
#'                             list(x = sim_prism, col = "blue")))
#'
#' #### Example (4): Customise spatial layers via additional arguments to each list
#' pretty_map(dat_gebco,
#'            add_raster = list(x = dat_gebco, col = viridis::viridis(100)))
#'
#' #### Example (4): Further customisation is possible via
#' # ... add_additional(), pretty_axis_args and ...
#' ## add_additional()
#' add_titles <- function(){
#'   mtext(side = 1, "x (UTM)", line = 2)
#'   mtext(side = 2, "y (UTM)", line = -8)
#' }
#' pretty_map(dat_gebco, add_additional = add_titles)
#' ## Similar (less tidy) implementation via ...
#' pretty_map(dat_gebco, xlab = "x (UTM)", ylab = "y (UTM)")
#' ## Fine-tune axes
#' pretty_map(dat_gebco, pretty_axis_args = list(side = 1:4))
#'
#' @author Edward Lavender
#' @export

pretty_map <- function(x = NULL,
                       add_rasters = NULL,
                       add_polys = NULL,
                       add_lines = NULL,
                       add_paths = NULL,
                       add_points = NULL,
                       add_additional = NULL,
                       crop_spatial = FALSE,
                       xlim = NULL, ylim = NULL,
                       pretty_axis_args =  list(side = 1:4,
                                                axis = list(list(),
                                                            list(),
                                                            list(labels = FALSE),
                                                            list(labels = FALSE)),
                                                control_sci_notation = list(magnitude = 16L, digits = 0)
                                                ),
                       verbose = TRUE
                       ,...){

  #### Checks
  t_onset <- Sys.time()
  cat_to_console <- function(..., show = verbose) if(show) cat(paste(..., "\n"))
  cat_to_console(paste0("prettyGraphics::pretty_map() called (@ ", t_onset, ")..."))
  cat_to_console("... Implementing function checks...")
  if(all(is.null(x), is.null(add_rasters), is.null(add_polys), is.null(add_lines), is.null(add_paths), is.null(add_points))) {
    stop("No spatial information provided via 'x' or any of the 'add_*' arguments. ")
  }
  if(!is.null(x) & is.null(add_rasters) & is.null(add_polys) & is.null(add_lines) & is.null(add_paths) & is.null(add_points)){
    message("'x' is the only spatial information provided: plotting the background only.")
  }

  #### Define area a list that contains spatial information (required below)
  ext <- x
  layers <- list(add_rasters, add_polys, add_lines, add_paths, add_points)
  if(list_depth(layers) > 2) layers <- purrr::flatten(layers)
  layers <- plyr::compact(layers)
  layers <- lapply(layers, function(layer) {
    if(inherits(layer, "list")) {
      out <- layer
    } else {
      out <- list(x = layer)
    }
    return(out)
    })
  names(layers) <- NULL

  #### Get CRS
  cat_to_console("... Getting CRS...")
  ## (1) Attempt to get the CRS from the object supplied to 'x'
  # ... This will return NA if an extent object of if the CRS is NULL
  # ... or if 'x' was not supplied, in which case we'll attempt to get the crs from the spatial layers
  if(!is.null(ext)) {
    area_crs <- raster::crs(ext)
    ext <- raster::extent(ext)
  } else area_crs <- NA
  ## (2) Attempt to get the CRS from spatial layers
  if(is.na(area_crs)){
    # Define a list of CRS strings from spatial layers
    layers_crs <- lapply(layers, function(layer){
      layer_crs <- tryCatch(raster::crs(layer$x), error = function(e) return(NULL))
      return(layer_crs)
    })
    layers_crs <- plyr::compact(layers_crs)
    # Generate a single CRS string
    if(length(layers_crs) == 0) {
      area_crs <- NA
    } else{
      check_crs_identical <- function(x) sum(duplicated.default(x)) == length(x) - 1L
      if(!check_crs_identical(layers_crs)) {
        message("Spatial layers do not have identical CRS strings")
      }
      area_crs <- layers_crs[[1]]
    }
  }
  message("CRS taken as: ", area_crs, ".")

  #### Get 'x' and 'y' coordinates used in pretty_axis from the ext object or the spatial layers
  # These will be used as xlim and ylim if x and ylim have not been supplied
  cat_to_console("... Getting axis parameters...")
  if(is.null(ext)){
    layers_xy <- lapply(layers, function(layer) {
      if(inherits(layer$x, "matrix")) {
        layer$y <- layer$x[, 2]
        layer$x <- layer$x[, 1]
      }
      return(pull_xy(layer$x, layer$y))
    })
    layers_x  <- lapply(layers_xy, function(layer_xy) layer_xy$x)
    layers_x  <- unlist(layers_x)
    layers_y  <- lapply(layers_xy, function(layer_xy) layer_xy$y)
    layers_y  <- unlist(layers_y)
    x <- range(layers_x)
    y <- range(layers_y)
  } else {
    y <- raster::extent(x)[3:4]
    x <- raster::extent(x)[1:2]
  }

  #### Define axes
  cat_to_console("... Defining pretty axes...")
  if(!is.null(ext) & !is.null(xlim)) ext[1:2] <- xlim
  if(!is.null(ext) & !is.null(ylim)) ext[3:4] <- ylim
  if(is.null(xlim)) xlim <- x
  if(is.null(ylim)) ylim <- y
  axis_param <- prettyGraphics::implement_pretty_axis_args(x = list(x, y),
                                                           pretty_axis_args = pretty_axis_args,
                                                           xlim = xlim,
                                                           ylim = ylim)

  #### Define area
  cat_to_console("... Defining area...")
  if(is.null(ext)) ext <- raster::extent(axis_param[[1]]$lim, axis_param[[2]]$lim)
  area <- sp::Polygon(ext)
  area <- sp::SpatialPolygons(list(sp::Polygons(list(area), ID = 1)))
  raster::crs(area) <- area_crs

  #### Define blank background map
  cat_to_console("... Plotting background...")
  raster::plot(area,
               xlim = axis_param[[1]]$lim, ylim = axis_param[[2]]$lim,
               axes = FALSE, col = NA,...)

  #### Add spatial layers
  if(any(!is.null(add_rasters),
         !is.null(add_polys),
         !is.null(add_lines),
         !is.null(add_paths),
         !is.null(add_lines),
         !is.null(add_points)
         )
  ){
    cat_to_console("... Adding spatial layer(s)...")
    ## Raster(s)
    if(!is.null(add_rasters)) {
      if(list_depth(add_rasters) == 1) add_rasters <- list(add_rasters)
      lapply(add_rasters, function(param){
        param$ext <- ext
        param$crop_spatial <- crop_spatial
        do.call(add_sp_raster, param)
      })
    }
    ## Polygon(s)
    if(!is.null(add_polys)) {
      if(list_depth(add_polys) == 1) add_polys <- list(add_polys)
      lapply(add_polys, function(param){
        param$ext <- ext
        param$crop_spatial <- crop_spatial
        do.call(add_sp_poly, param)
      })
    }
    ## Lines(s)
    if(!is.null(add_lines)) {
      if(list_depth(add_lines) == 1) add_lines <- list(add_lines)
      lapply(add_lines, function(param){
        param$ext <- ext
        param$crop_spatial <- crop_spatial
        do.call(add_sp_line, param)
      })
    }
    ## Paths(s)
    if(!is.null(add_paths)) {
      if(list_depth(add_paths) == 1) add_paths <- list(add_paths)
      lapply(add_paths, function(param){
        param$ext <- ext
        param$crop_spatial <- crop_spatial
        do.call(add_sp_path, param)
      })
    }
    ## Points(s)
    if(!is.null(add_points)) {
      if(list_depth(add_points) == 1) add_points <- list(add_points)
      lapply(add_points, function(param){
        param$ext <- ext
        param$crop_spatial <- crop_spatial
        do.call(add_sp_points, param)
      })
    }
  }

  ## Additional
  if(!is.null(add_additional)) {
    cat_to_console("... Evaluating add_additional()...")
    add_additional()
  }

  ### Add axes
  cat_to_console("... Adding pretty axes...")
  prettyGraphics::pretty_axis(axis_ls = axis_param, add = TRUE)
  t_end <- Sys.time()
  duration <- difftime(t_end, t_onset, units = "mins")
  cat_to_console(paste0("... prettyGraphics::pretty_map() call completed (@ ", t_end, ") after ~", round(duration, digits = 2), " minutes."))
  return(invisible())

}
