plot_numline <-
  function(x,
           pretty_axis_args = list(side = 1),
           inherit = NULL,
           side = NULL,
           pos = NULL,
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

      #### Replace side in pretty_axis_args, if requested
      if(!is.null(side)){
        if(!is.null(pretty_axis_args$side)) warning("Existing 'side' element in pretty_axis_args is overwritten since 'side' suppled to plot_line().")
        pretty_axis_args$side <- side
      }

      #### Check side has been supplied, otherwise force side = 1
      if(is.null(pretty_axis_args$side)){
        warning("pretty_axis_args$side is NULL; defaulting to side = 1.")
      }

      #### Define position of axis, if requested
      if(!is.null(pos)){
        # Issue a warning if pos is being overwritten.
        if("pos" %in% names(pretty_axis_args$axis)) warning("Existing 'pos' element in pretty_axis_args is overwritten since 'pos' suppled to plot_line().")
        pretty_axis_args$axis$pos <- pos
      } else{
        if(!is.null(pretty_axis_args$axis$pos)){
          pos <- pretty_axis_args$axis$pos
        } else{
          pos <- 0
          pretty_axis_args$axis$pos <- pos
        }
      }

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

      #### Replace side in axis_ls, if requested
      if(!is.null(side)){
        if(!is.null(axis_ls[[1]]$axis$side)) warning("Existing 'side' element in pretty_axis_args is overwritten since 'side' suppled to plot_line().")
        axis_ls[[1]]$axis$side <- side
      }

      #### Replace pos in axis_ls, if requested
      if(!is.null(side)){
        if(!is.null(axis_ls[[1]]$axis$pos)) warning("Existing 'side' element in pretty_axis_args is overwritten since 'side' suppled to plot_line().")
        axis_ls[[1]]$axis$pos <- pos
      }

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
    if(xtype){
      points(x, rep(pos, lx),...)
    } else{
      points(rep(pos, lx), x,...)
    }

  }
