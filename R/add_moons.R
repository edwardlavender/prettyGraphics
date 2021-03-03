#' @title Add subplots of the lunar phase to a plot
#' @description This function adds images of the lunar phase to lunar phase plots.
#'
#' @param side A numeric input that defines the side on to which you would like to add the moons. \code{side = 1}, \code{side = 2}, \code{side = 3}, \code{side = 4} add moons to the bottom, left, top and right axes respectively. The default is to add moons to the top of the plot. This is appropriate if the plot in question is a smooth of lunar phase. If the plot is a lunar phase time series, \code{side = 2} is more appropriate (see Examples).
#' @param pos A numeric input that defines the position at which moons will be added to the plot. If \code{side = 1} or \code{side = 3}, this refers to the height at which moons will be added. If \code{side = 2} or \code{side = 4}, this refers to the x value at which moons will be added (see Examples). This should be a single number; i.e. moons are all plotted at the same height (in the case of a smooth plot) or distance along the x axis (in the case of a time series plot).
#' @param outer A logical input that defines whether or not the moons will be drawn beyond the range of the x or y axis. If this is the case, \code{outer = TRUE}; otherwise, set \code{outer = FALSE}.
#' @param nv A numeric input that defines the number of vertices that are used to define moon shapes.
#' @param radius A numeric input that defines the radii of the moons added in user units. The default is 0.25.
#' @param units A character which defines the units of lunar phase on the existing plot. The default is \code{"radians"}. \code{"degrees"} is the other option.
#' @param ... Additional arguments: none are currently implemented but depreciated argument names ('radius1' and 'position') can be passed via \code{...}.
#'
#' @return Small subplots of the lunar phase (new moon, first quarter, full moon, third quarter, full moon) are added to an existing plot.
#'
#' @details This function requires the 'lunar' and 'plotrix' packages. Please install these before running this function, using: \code{install.packages(c("lunar", "plotrix").}
#'
#' @examples
#' #### Example (1): Add plots of lunar phase to simulated smooth function:
#' x <- seq(0, 2*pi, length.out = 100)
#' y <- (x - 10^2) + 10
#' axis_ls <- pretty_plot(x, y)
#' add_moons(side = 3,
#'           pos = axis_ls[[2]]$lim[2],
#'           outer = FALSE,
#'           nv = 100,
#'           radius = 0.1,
#'           units = "radians")
#'
#' #### Example (2): Add plots to a lunar phase time series
#' x <- seq.POSIXt(as.POSIXct("2016-01-01"), as.POSIXct("2016-05-01"), "days")
#' y <- lunar::lunar.phase(x)
#' axis_ls <- pretty_plot(x, y, type = "l")
#' add_moons(side = 2,
#'           pos = axis_ls[[1]]$lim[1],
#'           outer = TRUE,
#'           nv = 100,
#'           radius = 1e5,
#'           units = "radians")
#'
#' @export
#'

add_moons <-
  function(
    side = 1L,
    pos = 0,
    outer = TRUE,
    nv = 100,
    radius = 0.25,
    units = "radians",...
  ){

    #### Checks
    ## Check for requisite packages
    if(!requireNamespace("lunar", quietly = TRUE)){
      stop("Package \"lunar\" is needed for this function to work. Please install it.",
           call. = FALSE)
    }
    if(!requireNamespace("plotrix", quietly = TRUE)){
      stop("Package \"plotrix\" is needed for this function to work. Please install it.",
           call. = FALSE)
    }
    ## Check for depreciated arguments
    dots <- list(...)
    if("radius1" %in% names(dots)) radius <- dots$radius1
    if("position" %in% names(dots)) pos <- dots$position
    check_depreciated(c("radius1", "position"),...)

    #### Make moons

    # Adjust graphical options
    if(outer){
      # Save old user options (to restore at the end of the function)
      ouser <- options(stringsAsFactors = FALSE)
      # set par(xpd = NA)
      graphics::par(xpd = NA)
    }

    # Define the positions at which moons are to be plotted:
    moon_pos <- c(0, pi/2, pi, 3*pi/2, 2*pi)

    # Adjust positions, if the plot is in degrees
    if(units == "degrees") moon_pos <- moon_pos * (180/pi)

    # Define positions for moons:
    if(side == 1 | side == 3){
      # If the user has selected side 1 or 3, then x_position becomes the moon positions
      # ... and we'll repeate the position inputted to become the y_position
      x_position <- moon_pos
      y_position <- rep(pos, length(moon_pos))
    } else if(side == 2 | side == 4){
      # If the user has selected 2 or 4, the x_position is the inputted position
      # ... and the y_position becomes the inputted position
      x_position <- rep(pos, length(moon_pos))
      y_position <- moon_pos
    }

    # New moon
    plotrix::floating.pie(xpos = x_position[1], ypos = y_position[1],
                          edges = nv,
                          x = c(0, 2*pi),
                          radius = radius,
                          startpos = 0,
                          border = c("black"),
                          col = "black")

    # first quarter
    plotrix::floating.pie(xpos = x_position[2], ypos = y_position[2],
                          edges = nv,
                          x = c(pi, pi),
                          radius = radius,
                          startpos = pi/2,
                          border = c("black"),
                          col = c("black", "white"))

    # full moon
    plotrix::draw.circle(x = x_position[3], y = y_position[3], radius = radius, nv = nv)

    # 3rd quarter
    plotrix::floating.pie(xpos = x_position[4], ypos = y_position[4],
                          edges = nv,
                          x = c(pi, pi),
                          radius = radius,
                          startpos = pi/2,
                          border = "black",
                          col = c("white", "black"))

    # new moon
    plotrix::floating.pie(xpos = x_position[5], ypos = y_position[5],
                          edges = nv,
                          x = c(0, 2*pi),
                          radius = radius,
                          startpos = 0,
                          border = c("black"),
                          col = "black")

    # restore graphics properties if these have been modified
    if(outer) on.exit(options(ouser), add = TRUE)
    return(invisible())
  }
