#' @title Pull x and y coordinates from supported R objects for \code{\link[prettyGraphics]{pretty_axis}}
#' @description This function pulls 'x' and 'y' coordinates from some R objects. This is implemented within \code{\link[prettyGraphics]{pretty_plot}}, \code{\link[prettyGraphics]{pretty_axis}} to create axes for multiple object types.
#' @param x x coordinates or an object from which x and y coordinates can be extracted.
#' @param y y coordinates.
#' @return A list with 'x' and 'y' coordinates, extracted from the object \code{x} or as inputted.
#' @author Edward Lavender
#' @keywords internal
#'

pull_xy <-
  function(x, y = NULL){

    #### Checks
    warn <-  function(x, y){
      if(!is.null(y)) warning("y argument ignored when 'x' is an object of class ", class(x), ".")
    }

    ### Density
    if(inherits(x, "density")){
      warn(x, y)
      xc <- x$x
      yc <- x$y

    #### RasterLayer
    } else if(inherits(x, "RasterLayer")){
      warn(x, y)
      e <- raster::extent(x)
      xc <- e[1:2]
      yc <- e[3:4]

    #### Other
    } else{
      xc <- x
      yc <- y
    }

    #### Return list
    return(list(x = xc, y = yc))

  }
