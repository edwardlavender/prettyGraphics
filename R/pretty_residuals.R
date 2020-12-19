#' @title Diagnostic plots of model residuals
#'
#' @description This function produces (pretty) diagnostic plots of residuals. Plots can include standard diagnostic plots (i.e., residuals versus fitted values, residuals versus the linear predictor, a histogram of residuals and quantile-quantile plots), diagnostic plots for time series (i.e., time series of residuals and autocorrelation functions of residuals); and other helpful plots (i.e., residuals against factor levels/continuous covariates). For large datasets, these plots can be produced for random subsets of the data to aid interpretation.
#'
#' @param residuals A numeric vector of residuals from a model.
#' @param fv A numeric vector of fitted values from a model.
#' @param lp A numeric vector which defines the values of the linear predictor from a model.
#' @param vars A character vector which defines the names of variables in a dataframe (see \code{dat}, below) against which residuals will be plotted if \code{plot} includes option 5 (see below).
#' @param timestamp A character which defines the name of a variable in  \code{dat} which refers to timestamps. This is useful for models of data collected through time. If \code{NULL} and this plot is selected, \code{residuals} are plotted against an index for the selected data.
#' @param timestamp_fct (optional) A character which defines the name of a variable in \code{dat} which distinguishes independent time series. If \code{timestamp} is provided, it is desirable to plot the residuals ~ timestamp for only one of these levels (see \code{timestamp_fct_level}).
#' @param timestamp_fct_level An identifier of the independent time series in \code{timestamp_fct} to be plotted. If not provided, the function selects the longest time series.
#' @param dat A dataframe containing columns named as specified in \code{vars}. This should be the same dataframe that was used to fit the model from which residuals are extracted, although it can include extra variables not included in the model.
#' @param plot A numeric vector (1:7) which defines the plots to produce (see Details, below).
#' @param rand_pc A number which defines a percentage of residuals to plotted. If specified, a random subset of residuals, chosen according to a uniform distribution, are plotted. This is useful for some plots of residuals (e.g. residuals versus fitted values) which can be difficult to interpret with large datasets. However, note that some plots of residuals (e.g. quantile-quantile plots) respond poorly to selecting samples of residuals, and this option is not recommended in those cases - see \code{plot_rand_pc}, below.
#' @param plot_rand_pc A numeric input which defines which plots will use thinned residuals. This plots 3 - 6 by default (plots 1, 2 and 7 are still interpretable with large amounts of data.) The residual plot that corresponds to each plot number is explained in Details.
#' @param points_args A named list of arguments that is passed to \code{\link[graphics]{points}} to add points to appropriate plots.
#' @param lines_args A named list that is passed to \code{\link[graphics]{lines}} to add lines to appropriate plots.
#' @param pretty_axis_args A named list of arguments that is passed to \code{\link[prettyGraphics]{pretty_axis}} which is used to create pretty axes. For simplicity, this is implemented for most plots.
#' @param mtext_args A named list of arguments that is passed to \code{\link[graphics]{mtext}} to add labels to each plot. List names correspond to plot numbers (see Details). The default is a nested list which tries to add suitable labels in suitable locations to all plots, but this can be edited.
#'
#' @details Seven types of diagnostic plots can be produced: 1, a histogram of residuals; 2, a quantile-quantile plot; 3, residuals versus fitted values; 4, residuals versus linear predictor; 5, residuals against one or more user-defined variables; 6, residuals against a timestamp/index; 7, an autocorrelation function of residuals. \code{\link[prettyGraphics]{pretty_axis}} is used to control axes. This can be customised but changes affect all plots. Axis labels are implemented with \code{\link[graphics]{mtext}} via \code{mtext_args} to enable maximum user control over axes. The graphical characteristics of points and lines are specified in \code{points_args} and \code{lines_args}, respectively, and changes to these arguments affect all relevant plots. This implementation reflects a balance between user flexibility and simplicity.
#'
#' @return Diagnostic plots of residuals.
#'
#' @examples
#' #### Simulate and model data
#' set.seed(1)
#' x <- 1:1000
#' y <- rnorm(length(x), x*0.5, 30)
#' dat <- data.frame(x = x, y = y)
#' dat$fct <- sample(1:2, size = nrow(dat), replace = TRUE)
#' dat$z <- rnorm(nrow(dat), dat$x*0.01, 50)
#' m1 <- lm(y ~ x, data = dat)
#'
#' #### Plot residuals using default options
#' pp <- graphics::par(mfrow = c(3, 3))
#' pretty_residuals(residuals = stats::resid(m1),
#'                fv = fitted(m1),
#'                lp = fitted(m1),
#'                vars = c("z", "fct"),
#'                timestamp = "x",
#'                dat = dat,
#'                )
#' graphics::par(pp)
#'
#' #### Plot a single plot
#' pretty_residuals(residuals = stats::resid(m1), plot = 7)
#'
#' #### Customisation of axes is via pretty_axis_args; changes affect most plots.
#' pp <- graphics::par(mfrow = c(3, 3), oma = c(3, 3, 3, 3), mar = c(2, 2, 2, 2))
#' pretty_residuals(residuals = stats::resid(m1),
#'                  fv = fitted(m1),
#'                  lp = fitted(m1),
#'                  vars = c("z", "fct"),
#'                  timestamp = "x",
#'                  dat = dat,
#'                  pretty_axis_args = list(side = 1:2, pretty = list(n = 10))
#' )
#' graphics::par(pp)
#'
#' #### Customisation of graphics is via points_args() and lines_args()
#' # ... which are implemented for the relevant plots:
#' pp <- graphics::par(mfrow = c(3, 3), oma = c(3, 3, 3, 3))
#' pretty_residuals(residuals = stats::resid(m1),
#'                  fv = fitted(m1),
#'                  lp = fitted(m1),
#'                  vars = c("z", "fct"),
#'                  timestamp = "x",
#'                  dat = dat,
#'                  points_args = list(col = scales::alpha("red", 0.3)),
#'                  lines_args = list(lwd = 1, col = "skyblue")
#' )
#' graphics::par(pp)
#'
#' #### There are several customisation options for plotting observations against timestamps
#' # 'timestamp', 'timestamp_fct' and 'timestamp_fct_level' enable a specific time series to be
#' # ... plotted:
#' pretty_residuals(residuals = stats::resid(m1),
#'                  fv = fitted(m1),
#'                  lp = fitted(m1),
#'                  dat = dat,
#'                  plot = 6,
#'                  timestamp = "x",
#'                  timestamp_fct = "fct",
#'                  timestamp_fct_level = 1
#' )
#' # If 'timestamp' is not provided, an index is plotted:
#' pretty_residuals(residuals = stats::resid(m1),
#'                  fv = fitted(m1),
#'                  lp = fitted(m1),
#'                  dat = dat,
#'                  plot = 6,
#'                  timestamp = NULL,
#'                  timestamp_fct = "fct",
#'                  timestamp_fct_level = 1)
#' # If 'timestamp_fct' is not provided, data are assumed to comprise a single time series.
#' pretty_residuals(residuals = stats::resid(m1),
#'                  fv = fitted(m1),
#'                  lp = fitted(m1),
#'                  dat = dat,
#'                  plot = 6,
#'                  timestamp = "x",
#'                  timestamp_fct = NULL,
#' )
#' # If 'timestamp_fct_level' is not provided, the longest time series is chosen by default:
#' pretty_residuals(residuals = stats::resid(m1),
#'                  fv = fitted(m1),
#'                  lp = fitted(m1),
#'                  dat = dat,
#'                  plot = 6,
#'                  timestamp = "x",
#'                  timestamp_fct = "fct")
#'
#' @author Edward Lavender
#' @export


##############################################
##############################################
#### pretty_residuals()

pretty_residuals <-
  function(residuals,
           fv = NULL,
           lp = fv,
           vars = NULL,
           timestamp = NULL,
           timestamp_fct = NULL,
           timestamp_fct_level = NULL,
           dat = NULL,
           plot = 1:7,
           rand_pc = NULL,
           plot_rand_pc = 3:6,
           points_args = list(pch = 21,
                              col = scales::alpha("black", 0.3),
                              bg = scales::alpha("black", 0.3),
                              cex = 0.5
                              ),
           lines_args = list(col = scales::alpha("black", 0.9)),
           pretty_axis_args = list(side = 1:2, pretty = list(n = 5), axis = list(las = TRUE, cex.axis = 1.5)),
           mtext_args =
             list("1" = list(list(side = 1, text = "Residuals", line = 2.5),
                             list(side = 2, text = "Frequency", line = 2.5)
                             ),
                  "2" = list(list(side = 1, text = "Theoretical Quantiles", line = 2.5),
                             list(side = 2, text = "Sample Quantiles", line = 2.5)
                             ),
                  "3" = list(list(side = 1, text = "Fitted Values", line = 2.5),
                             list(side = 2, text = "Residuals", line = 2.5)
                             ),
                  "4" = list(list(side = 1, text = "Linear Predictor", line = 2.5),
                             list(side = 2, text = "Residuals", line = 2.5)
                             ),
                  "5" = lapply(vars, function(var){
                    list(list(side = 1, text = var, line = 2.5),
                         list(side = 2, text = "Residuals", line = 2.5)
                         )
                    }),
                  "6" = list(list(side = 1, text = "Timestamp", line = 2.5),
                             list(side = 2, text = "Residuals", line = 2.5)
                             ),
                  "7" = list(list(side = 1, text = "Lag", line = 2.5),
                             list(side = 2, text = "ACF", line = 2.5)
                             )
             )
  ){


    ##############################################
    #### Define dataframe and implement checks

    #### Check the residuals is the same length as the dataframe produced
    if(!is.null(dat) & !is.null(fv) & !is.null(lp)){
      stopifnot(all.equal(nrow(dat), length(residuals), length(fv), length(lp)))
    }
    if(is.null(dat)){
      dat <- data.frame(residuals = residuals)
    }
    dat$residuals <- residuals
    dat$fv <- fv
    dat$lp <- lp

    #### Check all required variables are included in the df
    if(!is.null(vars)){
      stopifnot(vars %in% colnames(dat))
    }
    if(!is.null(timestamp)){
      stopifnot(timestamp %in% colnames(dat))
    }
    if(!is.null(timestamp_fct)){
      stopifnot(timestamp_fct %in% colnames(dat))
      if(!is.null(timestamp_fct_level)) stopifnot(timestamp_fct_level %in% unique(dat[, timestamp_fct]))
    }


    ##############################################
    #### Processing

    #### Thin the residuals if requested
    if(!is.null(rand_pc)){
      # Check rand_pc is within the appropriate range
      if(rand_pc > 0 && rand_pc < 100){
        # define the number of residuals we'll select for this individual:
        nselect <- ceiling(nrow(dat)*(rand_pc/100))
        residuals_thin_pos <- sort(sample(1:nrow(dat), size = nselect, replace = FALSE, prob = NULL))
        dat_thin <- dat[residuals_thin_pos, ]
      # If rand_pc is not within the appropriate range, we'll stop:
      } else {
        stop("Invalid number for rand_pc specified. This must be specified such that: 0 < rand_pc < 100.")
      }
    } # close if(!is.null(rand_pc)){

    #### Define choose_dat function which chooses which data to use (dat or dat_thin)
    choose_dat <-
      function(n){
        if(!is.null(rand_pc) & n %in% plot_rand_pc){
          return(dat_thin)
        } else{
          return(dat)
        }
      }


    ##############################################
    #### (1) Histogram of residuals

    if(1 %in% plot){

      # Define ypretty
      if(plotrix::listDepth(pretty_axis_args$pretty) == 2){
        ypretty <- pretty_axis_args$pretty[[2]]$n
      } else{
        ypretty <- pretty_axis_args$pretty$n
      }
      if(is.null(ypretty)) ypretty <- 5

      # Define yaxis and xaxis
      yaxis <- list()
      xaxis <- list()
      axis_args <- list("tick", "line", "pos", "outer", "font", "lty", "lwd" , "lwd.ticks", "col", "col.ticks",
                        "hadj", "padj", "gap.axis", "cex.axis", "col.axis", "font.axis", "mgp", "xaxp", "yaxp",
                        "tck", "tcl", "las", "fg", "col", "xpd", "xaxt", "yaxt", "lab")
      if(is.list(pretty_axis_args$axis[[1]])){
        for(i in axis_args){
          if(!is.null(pretty_axis_args$axis[[1]][[i]])) xaxis[[i]] <- pretty_axis_args$axis[[1]][[i]]
          if(!is.null(pretty_axis_args$axis[[2]][[i]])) yaxis[[i]] <- pretty_axis_args$axis[[2]][[i]]
        }
      } else{
        for(i in axis_args){
          if(!is.null(pretty_axis_args$axis[[i]])) {
            xaxis[[i]] <- pretty_axis_args$axis[[i]]
            yaxis[[i]] <- pretty_axis_args$axis[[i]]
          }
        }
      }
      # Make histogram
      pretty_hist(choose_dat(1)$residuals,
                  xn = 2,
                  ypretty = list(n = ypretty),
                  xaxis = xaxis,
                  yaxis = yaxis,
                  mtext_args = mtext_args[["1"]])
    }


    ##############################################
    ##############################################
    #### (2) QQnorm plot

    if(2 %in% plot){
      qq <- stats::qqnorm(choose_dat(2)$residuals, plot = FALSE)
      qq_axis_ls <- pretty_plot(x = qq$x,
                                y = qq$y,
                                plot_xy = "y",
                                f = stats::qqnorm,
                                points_args = points_args,
                                pretty_axis_args = pretty_axis_args,
                                mtext_args = mtext_args[["2"]],
                                return_list = TRUE,
                                type = "n")
      usr <- graphics::par("usr")
      graphics::clip(qq_axis_ls[[1]]$lim[1], qq_axis_ls[[1]]$lim[2], qq_axis_ls[[2]]$lim[1], qq_axis_ls[[2]]$lim[2])
      qq_lines_args <- lines_args
      qq_lines_args$y <- choose_dat(2)$residuals
      do.call(stats::qqline, qq_lines_args)
      do.call("clip", as.list(usr))
    }




    ##############################################
    ##############################################
    #### (3) Residuals vs. fitted values

    if(3 %in% plot){
      pretty_plot(x = choose_dat(3)$fv,
                  y = choose_dat(3)$residuals,
                  plot_xy = "xy",
                  f = graphics::plot,
                  points_args = points_args,
                  pretty_axis_args = pretty_axis_args,
                  mtext_args = mtext_args[["3"]],
                  type = "n")
    }


    ##############################################
    ##############################################
    #### (4) Residuals vs. linear predictor

    if(4 %in% plot){
      pretty_plot(x = choose_dat(4)$lp,
                  y = choose_dat(4)$residuals,
                  plot_xy = "xy",
                  f = graphics::plot,
                  points_args = points_args,
                  pretty_axis_args = pretty_axis_args,
                  mtext_args = mtext_args[["4"]],
                  type = "n")
    }



    ##############################################
    ##############################################
    #### (5) Residuals vs. factors/covariates (excluding timestamp)

    if(5 %in% plot){
      if(!is.null(vars)){
        mply <-
          mapply(1:length(vars), vars, FUN = function(j, var){
            df <- choose_dat(5)[, c("residuals", var)]
            pretty_plot(x = df[, var],
                        y = df[, "residuals"],
                        plot_xy = "xy",
                        f = graphics::plot,
                        points_args = points_args,
                        pretty_axis_args = pretty_axis_args,
                        mtext_args = mtext_args[["5"]][[j]],
                        type = "n")
          })
      } else{
       warning("plot 5 residuals ~ vars is not implemented because vars = NULL.")
      }
    }

    ##############################################
    ##############################################
    #### (6) residuals versus timestamp

    if(6 %in% plot){

      #### Define data
      d6 <- choose_dat(6)

      #### Select data for specific individual
      ## a) timestamp_fct is NULL so assuming one individual
      if(is.null(timestamp_fct)){
        message("plot (6) residuals ~ timestamp: 'timestamp_fct' is NULL; assuming 'dat' only contains one independent time series.")
        if(!is.null(timestamp_fct_level)) warning("'timestamp_fct' is NULL so 'timestamp_fct_level' is ignored.")

        ## b) select data for specific individaul
      } else{
        # i) Define timestamp_fct_level using the individual with the longest time series, if not provided
        if(is.null(timestamp_fct_level)){
          message("plot (6) residuals ~ timestamp: selecting the 'timestamp_fct' with the longest time series.")
          d6_ls <- split(d6, f = d6[, timestamp_fct])
          d6_longest <- which.max(sapply(d6_ls, nrow))
          timestamp_fct_level <- d6_ls[[d6_longest]][1, timestamp_fct]
        }
        # ii) Select appropriate data
        d6 <- d6[d6[, timestamp_fct] == timestamp_fct_level, ]
      }

      #### Define timestamps if necessary
      if(is.null(timestamp)){
        message("plot (6) residuals ~ timestamp: timestamp is NULL so using an index for selected data.")
        d6$index <- 1:nrow(d6)
        timestamp <- "index"
      }

      #### Check that data are sorted by timestamp
      if(is.unsorted(d6[, timestamp])){
        warning("plot (6) residuals ~ timestamp: selected data are not sorted by timestamp. 'dat' needs to be ordered by fct_timestamp (if applicable) then timestamp.")
      }

      #### Plot
      pretty_plot(x = d6[, timestamp],
                  y = d6[, "residuals"],
                  f = graphics::plot,
                  plot_xy = "xy",
                  points_args = points_args,
                  lines_args = lines_args,
                  pretty_axis_args = pretty_axis_args,
                  mtext_args = mtext_args[["6"]],
                  type = "n")
    }


    ##############################################
    ##############################################
    #### (7) ACFs

    if(7 %in% plot){
      acf_obj <- stats::acf(choose_dat(7)$residuals, plot = FALSE)
      pretty_acf_ls <- pretty_plot(x = 1:length(acf_obj$acf),
                                   y = as.numeric(acf_obj$acf),
                                   plot_xy = "xy",
                                   f = graphics::plot,
                                   pretty_axis_args = pretty_axis_args,
                                   mtext_args = mtext_args[["7"]],
                                   return_list = TRUE,
                                   type = "n"
      )

      wnlimit <- stats::qnorm((1 + 0.95)/2)/sqrt(acf_obj$n.used)
      wnupper <- 0 + wnlimit
      wnlower <- 0 - wnlimit
      graphics::polygon(c(pretty_acf_ls[[1]]$lim, rev(pretty_acf_ls[[1]]$lim)),
                        c(rep(wnlower, 2), rep(rev(wnupper), 2)),
                        col = scales::alpha("lightgrey", 0.8),
                        border = NA)
      points_args_p7   <- points_args
      points_args_p7$x <- 1:length(acf_obj$acf)
      points_args_p7$y <- as.numeric(acf_obj$acf)
      lines_args_p7    <- lines_args
      lines_args_p7$x  <- 1:length(acf_obj$acf)
      lines_args_p7$y  <- as.numeric(acf_obj$acf)
      do.call(graphics::points, points_args_p7)
      do.call(graphics::lines, lines_args_p7)
    }


  } # close function



#### End of code.
##############################################
##############################################
