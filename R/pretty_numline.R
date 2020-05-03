#' @title Plot a (pretty) number line or timeline
#' @description This function produces a one dimensional (pretty) number line or timeline onto which observed data are added. The line is drawn by \code{\link[plot.pretty]{pretty_axis}}. The function can produce a line as an independent plot, or add the line/points to an existing plot. In the latter case, rather than re-defining a new axis, a list of axis parameters for the existing plot (i.e. returned by \code{\link[plot.pretty]{pretty_axis}}) can be inherited and, if necessary, modified, before its addition to an existing plot.
#'
#' @param x A vector of observed data. Numeric/integer, factor, date, or date-time objects are supported.
#' @param pretty_axis_args A named list of arguments that is passed to \code{\link[plot.pretty]{pretty_axis}} which is used to draw the number line or timeline. If \code{axis_ls} is supplied, \code{inherit} and \code{replace_axis} can be used to modify this element before the line is drawn, if desired.
#' @param inherit (optional) If the \code{axis_ls} argument is supplied to \code{pretty_axis_args}, \code{inherit} can be supplied. This is a single integer which specifies which element of the \code{axis_ls} list should be used. This is useful because the \code{axis_ls} argument returned by \code{\link[plot.pretty]{pretty_axis}} will usually have multiple elements (one for each axis on an existing plot); but for one dimensional number lines or timelines, only one of these element is relevant
#' @param replace_axis (optional) If the \code{axis_ls} argument is supplied to \code{pretty_axis_args}, \code{replace_axis} can be  supplied. This is a named list of arguments that is used to replace corresponding arguments (of the same name) in an inherited list (i.e., the inherited element of \code{axis_ls}, if supplied). This allows some components of an inherited list (e.g. tick marks) to be retained, while others (e.g. axis position) are easily adjusted.
#' @param add A logical input which defines whether not the line should be added to an existing plot (\code{TRUE}) or not (\code{FALSE}).
#' @param return_list A logical input which defines whether or not to return the list produced by \code{\link[plot.pretty]{pretty_axis}}.
#' @param ... Additional arguments passed to \code{\link[graphics]{points}}, which is used to add observed data (\code{x}) to the line.
#'
#' @return The function produces a number line or a timeline. If \code{return_list = TRUE}, the function also returns the list of axis parameters defined by \code{\link[plot.pretty]{pretty_axis}}.
#'
#' @examples
#'
#' @author Edward Lavender
#' @export
#'

############################################
############################################
#### pretty_numline()

pretty_numline <-
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

      #### Add x to pretty_axis_args
      if(is.null(pretty_axis_args$x)) pretty_axis_args$x <- list(x)

      #### Warn if inherit has been supplied
      if(!is.null(inherit)) warning("pretty_axis_args$axis_ls is NULL; therefore, 'inherit' argument is ignored.")

      #### Warn if replace_axis list has been supplied
      if(!is.null(replace_axis)) warning("pretty_axis_args$axis_ls is NULL; therefore, 'replace_list' argument is ignored")

      #### Check side has been supplied, otherwise force side = 1
      if(is.null(pretty_axis_args$side)){
        warning("pretty_axis_args$side is NULL; defaulting to side = 1.")
      }

      #### Force position of axis, if necessary
      if(!("pos" %in% names(pretty_axis_args$axis))) pretty_axis_args$axis$pos <- 0

      #### Implement pretty_axis_args
      axis_ls <- implement_pretty_axis_args(pretty_axis_args)

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
        if(plotrix::listDepth(pretty_axis_args$axis_ls) > 1){
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
