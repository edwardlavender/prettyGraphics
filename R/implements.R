#######################################
#######################################
#### implement_pretty_axis_args

#' @title Implement \code{pretty_axis_args}
#' @description This function implements the \code{pretty_axis_args} argument in functions.
#' @param x A list of of coordinates (see \code{\link[prettyGraphics]{pretty_axis}}).
#' @param pretty_axis_args A named list of parameters that are passed to \code{\link[prettyGraphics]{pretty_axis}}.
#' @param xlim,ylim A vector of x and y axis limits that are passed to \code{\link[prettyGraphics]{pretty_axis}}. These can also be supplied via \code{pretty_axis_args}, but supplying \code{xlim} and \code{ylim} can be more convenient.
#' @param ... Arguments passed to or from other methods.
#' @return A list, returned by \code{\link[prettyGraphics]{pretty_axis}}.
#' @author Edward Lavender
#' @export
#'
implement_pretty_axis_args <-
  function(x, pretty_axis_args = NULL, xlim = NULL, ylim = NULL,...){

    #### Set up pretty_axis_args
    ## Define list
    if(is.null(pretty_axis_args)) pretty_axis_args <- list()
    ## Add x to pretty_axis_args if not supplied
    if(length(pretty_axis_args$x) == 0) pretty_axis_args$x <- x
    ## Add limits, if supplied
    if(is.null(pretty_axis_args$lim)) pretty_axis_args$lim <- list(x = NULL, y = NULL)
    if(!is.null(xlim)){
      # Drop attributes
      xlim <- as.numeric(xlim)
      # Add to pretty_axis_args
      if(!is.null(pretty_axis_args$lim[1][[1]])) warning("pretty_axis_args$lim[[1]] replaced by input to 'xlim'.")
      pretty_axis_args$lim[[1]] <- xlim
    }
    if(!is.null(ylim)){
      ylim <- as.numeric(ylim)
      if(!is.null(pretty_axis_args$lim[2][[1]])) warning("pretty_axis_args$lim[[2]] replaced by input to 'ylim'.")
      pretty_axis_args$lim[[2]] <- ylim
    }

    #### If pretty axis parameters have been supplied...
    if(length(pretty_axis_args) > 0){

      #### Define default parameters:
      paa <- list(
        side = 1:2,
        pretty = list(n = 5),
        axis_ls = NULL,
        add = FALSE,
        return_list = NULL,...
        )

      #### Check: has 'side' been supplied?
      # Check side has been supplied: if it has, remove side from paa, otherwise print warning
      # ... and default to side = 1:2
      if("side" %in% names(pretty_axis_args) | "axis_ls" %in% names(pretty_axis_args)){
        paa$side <- NULL
        if("side" %in% names(pretty_axis_args)){
          if(length(pretty_axis_args$side) == 1) pretty_axis_args$lim <- pretty_axis_args$lim[1]
        } else{
          if(length(names(pretty_axis_args$axis_ls) == 1)) pretty_axis_args$lim <- pretty_axis_args$lim[1]
        }
      } else{
        message("Argument 'side' not supplied to pretty_axis_args (nor 'axis_ls'); defaulting to side = 1:2.")
      }

      #### Check: has 'pretty' been supplied?
      # Remove pretty argument in default list if supplied;
      # This prevents issues when two lists are provided, in which case the user-provided argument remains.
      if("pretty" %in% names(pretty_axis_args)) paa$pretty <- NULL
      # Likewise, remove pretty if units are supplied:
      if("units" %in% names(pretty_axis_args)) paa$pretty <- NULL

      #### Merge parameters
      pretty_axis_args <- rlist::list.merge(paa, pretty_axis_args)

      #### Define list
      axis_ls <- do.call(pretty_axis, pretty_axis_args)

      #### Return list
      return(axis_ls)
    }
  }


#######################################
#######################################
#### implement_mtext_args()

#' @title Implement \code{mtext_args}
#' @description This function implements the \code{mtext_args} argument in functions.
#' @param mtext_args A named list.
#' @return The function adds axes labels to a plot via \code{\link[graphics]{mtext}}.
#' @author Edward Lavender
#' @export

implement_mtext_args <-
  function(mtext_args){
    if(length(mtext_args) > 0){
      invisible(
        lapply(mtext_args, function(mta){
          do.call(graphics::mtext, mta)
          })
      )
    }
  }


#### End of code.
#######################################
#######################################
