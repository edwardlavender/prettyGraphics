#' @title Pull x and y coordinates from supported R objects for pretty plotting functions
#' @description This function pulls 'x' and 'y' coordinates from some R objects. This is implemented within \code{\link[prettyGraphics]{pretty_axis}} and \code{\link[prettyGraphics]{pretty_plot}} to create axes for multiple object types. Suggested x and y axis labels are also returned: these are NULL if automatically defined labels are deemed appropriate or character strings which can be used to replace automatically defined labels.
#' @param x x coordinates or an object from which x and y coordinates can be extracted.
#' @param y y coordinates.
#' @return A list with 'x' and 'y' coordinates, extracted from the object \code{x} or as inputted, as well as 'xlab' and 'ylab' labels, if manual labels are desirable.
#' @author Edward Lavender
#' @keywords internal
#'

pull_xy <-
  function(x, y = NULL){

    #### Checks
    warn <-  function(x, y){
      if(!is.null(y)) warning("y argument ignored when 'x' is an object of class ", class(x), ".")
    }

    #### Suggest axis labels
    xlab <- NULL
    ylab <- NULL

    #### Density
    if(inherits(x, "density")){
      warn(x, y)
      xc <- x$x
      yc <- x$y
      xlab <- "x"
      ylab <- "Density"

    #### Spatial* objects supported by raster::extent()
    } else if(inherits(x, c("RasterLayer", "Line", "Lines", "Polygon", "Polygons", "SpatialPolygonsDataFrame"))){
      warn(x, y)
      e <- raster::extent(x)
      xc <- e[1:2]
      yc <- e[3:4]
      xlab <- "x"
      ylab <- "y"

    #### Other
    } else{
      xc <- x
      yc <- y
    }

    #### Return list
    out <- list(x = xc,
                y = yc,
                xlab = xlab,
                ylab = ylab)
    return(out)

  }
