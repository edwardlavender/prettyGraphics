#' @title Plot a (pretty) number line or timeline
#' @description This function produces a one dimensional (pretty) number line or timeline onto which observed data are added. The line is drawn by \code{\link[prettyGraphics]{pretty_axis}}. The function can produce a line as an independent plot, or add the line/points to an existing plot. In the latter case, rather than re-defining a new axis, a list of axis parameters for the existing plot (i.e. returned by \code{\link[prettyGraphics]{pretty_axis}}) can be inherited and, if necessary, modified, before its addition to an existing plot.
#'
#' @param x A vector of observed data. Numeric/integer, factor, date, or date-time objects are supported.
#' @param pretty_axis_args A named list of arguments that is passed to \code{\link[prettyGraphics]{pretty_axis}} which is used to draw the number line or timeline. If \code{axis_ls} is supplied, \code{inherit} and \code{replace_axis} can be used to modify this element before the line is drawn, if desired.
#' @param inherit (optional) If the \code{axis_ls} argument is supplied to \code{pretty_axis_args}, \code{inherit} can be supplied. This is a single integer which specifies which element of the \code{axis_ls} list should be used. This is useful because the \code{axis_ls} argument returned by \code{\link[prettyGraphics]{pretty_axis}} will usually have multiple elements (one for each axis on an existing plot); but for one dimensional number lines or timelines, only one of these element is relevant.
#' @param replace_axis (optional) If the \code{axis_ls} argument is supplied to \code{pretty_axis_args}, \code{replace_axis} can be  supplied. This is a named list of arguments that is used to replace corresponding arguments (of the same name) in an inherited list of axis argument (i.e., the inherited 'axis' of \code{axis_ls}, if supplied). This allows some components of an inherited list (e.g. tick marks) to be retained, while others (e.g. axis position) are easily adjusted.
#' @param add A logical input which defines whether not the line should be added to an existing plot (\code{TRUE}) or not (\code{FALSE}).
#' @param return_list A logical input which defines whether or not to return the list produced by \code{\link[prettyGraphics]{pretty_axis}}.
#' @param ... Additional arguments passed to \code{\link[graphics]{points}}, which is used to add observed data (\code{x}) to the line.
#'
#' @return The function produces a number line or a timeline. If \code{return_list = TRUE}, the function also returns the list of axis parameters defined by \code{\link[prettyGraphics]{pretty_axis}}.
#'
#' @examples
#' #### Generate some example (numeric) data for example number lines
#' set.seed(1)
#' x <- runif(5, 0, 10)
#' y <- runif(5, 0, 10)
#'
#' #### Example (1): Plot a number line using default options
#' pretty_line(x)
#'
#' #### Example (2): Customise points via ...
#' pretty_line(x, pch = 21, bg = "red")
#'
#' #### Example (2): Customise the number line via pretty_axis_args
#' pretty_line(x, pretty_axis_args = list(side = 2, axis = list(las = TRUE)),
#'                pch = 21, bg = "red")
#'
#' #### Example (3): Add a number line to an existing plot:
#' # Create plot
#' axis_ls <- pretty_plot(x, y, return_list = TRUE)
#' # Method 1: manually specify pretty_axis_args as desired and specify add = TRUE:
#' pretty_line(x, pretty_axis_args = list(side = 3,
#'                                           axis = list(pos = axis_ls[[2]]$lim[2])),
#'                add = TRUE,
#'                pch = 21, bg = "red")
#' # Method 2: specify axis_ls argument to pretty_axis_args and, if applicable, inherit.
#' # In this situation, necessary arguments (e.g. side) can be replaced via replace_axis
#' # ... while the properties of the axis (i.e., labels, colour etc.) are maintained:
#' axis_ls <- pretty_plot(x, y, return_list = TRUE)
#' pretty_line(x, pretty_axis_args = list(axis_ls = axis_ls),
#'                # select the  first element of axis_ls i.e. axis_ls[[1]]
#'                inherit = 1,
#'                # replace the following arguments in axis_ls[[1]]$axis while retaining remaining
#'                # ... arguments:
#'                replace_axis = list(side = 3, pos = 5, labels = FALSE, lwd.ticks = 0),
#'                add = TRUE,
#'                pch = 21, bg = "red")
#'
#'
#' #### Example (4): An examine with time-series data i.e., a timeline
#' set.seed(1)
#' tseq <- seq.POSIXt(as.POSIXct("2017-01-01", tz = "UTC"),
#'                    as.POSIXct("2018-01-01", tz = "UTC"), by = "days")
#' pretty_line(sample(tseq, size = 10), pch = 21, bg = "red")
#'
#'
#' @author Edward Lavender
#' @export
#'

############################################
############################################
#### pretty_line()

pretty_line <-
  function(x,
           pretty_axis_args = list(side = 1),
           inherit = NULL,
           replace_axis = NULL,
           add = FALSE,
           return_list = FALSE,
           ...){

    #### Option (1): pretty_axis_args does not contain axis_ls
    # In this case, we need to define axis_ls.
    if(is.null(pretty_axis_args$axis_ls)) pretty_axis_args$axis_ls <- NULL
    if(is.null(pretty_axis_args$axis_ls) | length(pretty_axis_args$axis_ls) == 0){

      #### Warn if inherit has been supplied
      if(!is.null(inherit)) warning("pretty_axis_args$axis_ls is NULL; therefore, 'inherit' argument is ignored.")

      #### Warn if replace_axis list has been supplied
      if(!is.null(replace_axis)) warning("pretty_axis_args$axis_ls is NULL; therefore, 'replace_list' argument is ignored.")

      #### Check side has been supplied, otherwise force side = 1
      if(is.null(pretty_axis_args$side)){
        message("pretty_axis_args$side is NULL; defaulting to side = 1.")
      }

      #### Force position of axis, if necessary
      if(!("pos" %in% names(pretty_axis_args$axis))) pretty_axis_args$axis$pos <- 0

      #### Implement pretty_axis_args
      axis_ls <- implement_pretty_axis_args(list(x), pretty_axis_args)

    #### Option (2): axis_ls has been supplied, but may require adjustments via side or pos.
    } else{

      #### Extract axis_ls from pretty_axis_args
      axis_ls <- pretty_axis_args$axis_ls

      #### Select specific element of axis_ls, if inherited from
      # ... output of previous call to pretty_axis()
      if(!is.null(inherit)){
        if(length(inherit) != 1){ stop("'inherit' argument should be a single number.")}
        axis_ls <- axis_ls[inherit]
      } else{
        if(list_depth(pretty_axis_args$axis_ls) > 1){
          warning("pretty_axis_args$axis_ls contains multiple lists, but 'inherit' is not supplied; defaulting to axis_ls[1], but this may not position the timeline correctly.")
          axis_ls <- axis_ls[1]
        }
      }

      #### Replace elements in axis_ls, if necessary
      if(!is.null(replace_axis)) axis_ls[[1]]$axis <- rlist::list.merge(axis_ls[[1]]$axis, replace_axis)

    }

    #### Determine whether the number/time line will be added in the x or y direction
    if(axis_ls[[1]]$axis$side %in% c(1, 3)) xtype <- TRUE else xtype <- FALSE

    #### Create blank plot, if number/time line is not added to an existing plot
    if(!add){
      pls <- list(x = 0, y = 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "")
      if(xtype){
        pls$xlim <- axis_ls[[1]]$lim
      } else{
        pls$ylim <- axis_ls[[1]]$lim
      }
      do.call(graphics::plot, pls)
    }

    #### Add axes
    pretty_axis(axis_ls = axis_ls, add = TRUE)

    #### Add points to number/timeline
    lx <- length(x)
    pos <- axis_ls[[1]]$axis$pos
    if(xtype){
      graphics::points(x, rep(pos, lx),...)
    } else{
      graphics::points(rep(pos, lx), x,...)
    }


    #### Return list
    if(return_list) return(axis_ls)

  }

#### End of code.
############################################
############################################
