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
      # Default parameters
      paa <- list(
        side = 1,
        # x = list(x = x, y = y),
        # lim = list(NULL),
        pretty = list(n = 5),
        # units = list(NULL),
        # axis = list(NULL),
        axis_ls = NULL,
        add = FALSE,
        return_list = TRUE
      )
      # Merge parameters
      pretty_axis_args <- rlist::list.merge(paa, pretty_axis_args)
      # Add list(NULL) if necessary
      utils.add::add_list_null(pretty_axis_args, c("lim", "units", "axis"))
      # Define list
      axis_ls <- do.call("pretty_axis", pretty_axis_args)
      # return list
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
