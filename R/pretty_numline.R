pretty_numline <-
  function(x,
           pretty_axis_args = list(side = 1),
           inherit = NULL,
           replace_axis = NULL,
           add = FALSE,
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
      points(x, rep(pos, lx),...)
    } else{
      points(rep(pos, lx), x,...)
    }

  }
