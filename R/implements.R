#######################################
#######################################
#### implement_pretty_axis_args

#' @title Implement \code{pretty_axis_args}
#' @description This function implements the pretty_axis_args argument in functions. It is not intended to be called directly by the user.
#' @param pretty_axis_args A named list of parameters that are passed to \code{\link[plot.pretty]{pretty_axis}}.
#' @return A list.
#' @author Edward Lavender
#' @export

implement_pretty_axis_args <-
  function(pretty_axis_args){
    if(length(pretty_axis_args) > 0){

      #### Define default parameters:
      paa <- list(
        side = 1:2,
        pretty = list(n = 5),
        axis_ls = NULL,
        add = FALSE,
        return_list = TRUE
        )

      #### Check: has 'side' been supplied?
      # Check side has been supplied: if it has, remove side from paa, otherwise print warning
      # ... and default to side = 1:2
      if("side" %in% names(pretty_axis_args) & !("axis_ls" %in% names(pretty_axis_args))){
        paa$side <- NULL
      } else{
        warning("Argument 'side' not supplied to pretty_axis_args; defaulting to side = 1:2.")
      }

      #### Check: has 'pretty' been supplied?
      # Remove pretty argument if supplied;
      # This prevents issues when two lists are provided, in which case the original
      # ... list will remain.
      if("pretty" %in% names(pretty_axis_args)) paa$pretty <- NULL
      # Likewise, remove pretty if units are supplied:
      if("units" %in% names(pretty_axis_args)) paa$pretty <- list(NULL)

      #### Merge parameters
      pretty_axis_args <- rlist::list.merge(paa, pretty_axis_args)
      # Add list(NULL) if necessary
      utils.add::add_list_null(pretty_axis_args, c("lim", "units", "axis"))

      #### Define list
      axis_ls <- do.call("pretty_axis", pretty_axis_args)

      #### Return list
      return(axis_ls)
    }
  }


#######################################
#######################################
#### implement_mtext_args()

#' @title Implement \code{mtext_args}
#' @description This function implements the \code{mtext_args} argument in functions. It is not intended to be called directly by the user.
#' @param mtext_args A named list.
#' @return A list.
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
