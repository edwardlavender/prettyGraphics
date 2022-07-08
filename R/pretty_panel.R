#' @title Pretty grouped, multi-panel plots
#' @description This function creates pretty multi-panel plots of \code{y} ~ \code{x} split by a grouping variable (\code{by}).
#' @param x,y,by Vectors that define point coordinates (\code{x, y}) and a grouping variable (\code{by}).
#' @param data (optional) A dataframe that contains additional information (see \code{add_additional}).
#' @param xlim,ylim,pretty_axis_args Axis controls. \code{xlim} and \code{ylim} control axis limits for all plots. \code{pretty_axis_args} is a named list of arguments, passed to \code{\link[prettyGraphics]{pretty_axis}}, for further control.
#' @param add_xlab,add_ylab,add_main (optional) Named lists of arguments, passed to \code{\link[graphics]{mtext}}, to add axis titles to plots. Panel titles are added to each panel, while only  global labels are added for the x and y axes. Empty lists specify default arguments. In this case, panel titles are given as capitalised letters or numbers (if there are more than 26 predictors), in bold, plus the grouping level in brackets; e.g., \strong{A} (Group One) or \strong{1} (Group One). X and y axis labels are simply given as \code{x} and \code{y}. Alternatively, names can be specified via the `text' argument to \code{\link[graphics]{mtext}}. \code{NULL} suppress these arguments.
#' @param add_additional A function used to add additional elements to each panel. This must accept two arguments, even if they are ignored: (1) a list of axis parameters (from \code{\link[prettyGraphics]{pretty_axis}}) and (2) the group-specific dataframe (see examples).
#' @param par_param A list of arguments for \code{\link[graphics]{par}} to customise the plotting window. If unsupplied, default settings defined inside the function are used.
#' @param by_row A logical variable that defines whether to order panels by row (\code{TRUE}) or column (\code{FALSE}).
#' @param ... Additional arguments passed to \code{\link[prettyGraphics]{pretty_plot}}.
#' @return The function produces a multi-panel plot.
#' @examples
#' #### Example (1): Implement function using default options
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species)
#'
#' #### Example (2): Control axes via pretty_axis_args, xlim and ylim
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species,
#'              pretty_axis_args = list(side = 1:4),
#'              by_row = FALSE)
#'
#' #### Example (3): Control par via par_param and by_row
#' ## Example using par
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species,
#'              par_param = list(oma = rep(6, 4)))
#'
#' \dontrun{
#' # Par arguments are reset unless supplied via par_param,
#' # ... so the following does not (currently) work:
#' pp <- graphics::par(oma = rep(6, 4))
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species)
#' graphics::par(pp)
#' }
#'
#' ## Example with by_row
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species,
#'              by_row = FALSE)
#'
#' #### Example (4): Add titles
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species,
#'              add_xlab = list(text = "Length", line = 2),
#'              add_ylab = list(text = "Width", line = 2),
#'              add_main = list(adj = 0))
#'
#' #### Example (5): Use add_additional(...) and ... to add additional elements
#' ## Scenario: We will plot predictions from a model of sepal width ~ length for each spp
#' # Fit model
#' mod <- lm(Sepal.Width ~ Sepal.Length * Species, data = iris)
#' # Define function to add predictions that depends on:
#' # ... axis parameters
#' # ... group- (species-) specific data
#' add_pred <- function(.axis_ls = NULL, .data){
#'   n    <- 100
#'   xlim <- range(.data$Sepal.Length)
#'   nd <- data.frame(Sepal.Length = seq(xlim[1], xlim[2], length.out = n),
#'                    Species = factor(rep(.data$Species[1], n),
#'                                     levels = levels(iris$Species)))
#'   p <- predict(mod, newdata = nd, se.fit = TRUE)
#'   ci <- list_CIs(p)
#'   add_error_envelope(x = nd$Sepal.Length, ci = ci)
#'   points(.data$Sepal.Length, .data$Sepal.Width)
#' }
#' # Make plot
#' pretty_panel(iris$Sepal.Length, iris$Sepal.Width, iris$Species,
#'              data = iris,
#'              add_additional = add_pred,
#'              type = "n")
#'
#' @author Edward Lavender
#' @export

pretty_panel <-
  function(x, y, by, data = NULL,
           xlim = NULL, ylim = NULL,
           pretty_axis_args = list(side = 1:2,
                                   control_axis = list(tck = 0.025, las = TRUE)),
           add_xlab = NULL,
           add_ylab = NULL,
           add_main = NULL,
           add_additional = NULL,
           par_param = NULL, by_row = TRUE,...){

    #### Define 'data'
    if(is.null(data)){
      data <- data.frame(x = x, y = y, group = by)
    } else {
      if(any(c("x", "y", "group") %in% colnames(data))){
        warning("columns 'x', 'y' and/or 'group' in 'data' overwritten.",
                call. = FALSE, immediate. = FALSE)
      }
      data$x <- x
      data$y <- y
      data$group <- by
    }
    n_groups <- length(unique(data$group))
    if(inherits(data$group, "factor") && length(levels(data$group)) != n_groups){
      warning("Empty group factor level(s) ignored.",
              call. = FALSE, immediate. = FALSE)
      data$group <- factor(data$group,
                           levels = levels(data$group)[levels(data$group) %in% data$group])
    }

    #### Define plotting window
    par_param_init <- graphics::par(no.readonly = TRUE)
    on.exit(do.call(graphics::par, par_param_init), add = TRUE)
    if(is.null(par_param)) par_param = list()
    if(is.null(par_param$oma)) par_param$oma <- c(5, 5, 2, 2)
    if(is.null(par_param$mar)) par_param$mar <- c(0, 0, 0, 0)
    if(is.null(par_param$xaxs)) par_param$xaxs <- "i"
    if(is.null(par_param$yaxs)) par_param$yaxs <- "i"
    if(!is.null(par_param$mfrow) & !by_row){
      warning("'par_param$mfrow' set but 'by_row' = FALSE.", call. = FALSE)
      par_param$mfrow <- NULL
    }
    if(!is.null(par_param$mfcol) & by_row){
      warning("'par_param$mfcol' set but 'by_row' = TRUE", call. = FALSE)
      par_param$mfcol <- NULL
    }
    if(is.null(par_param$mfrow) & is.null(par_param$mfcol)){
      mf <- par_mf(n_groups)
      if(by_row) par_param$mfrow <- mf else par_param$mfcol <- mf
    }
    pp <- do.call(graphics::par, par_param)

    #### Define global param
    paa_box      <- pretty_axis_args
    if(is.null(paa_box$x)) paa_box$x <- list(x = data$x, y = data$y)
    paa_box$side <- 1:4
    paa_box$add  <- FALSE

    #### Loop over each group and plot
    data_by_group <- split(data, data$group)
    lapply(1:length(data_by_group), function(i){

      ## Define plot param
      # i <- 1
      d <- data_by_group[[i]]

      ## Define blank plot
      axis_ls <- pretty_plot(d$x, d$y,
                             xlim = xlim, ylim = ylim,
                             pretty_axis_args = paa_box,...)

      ## Add additional elements
      if(!is.null(add_additional)) add_additional(axis_ls, d)

      ## Add blank axes
      axis_ls_marks <-
        lapply(axis_ls, function(elm) {
          elm$axis$labels <- FALSE
          return(elm)
        })
      pretty_axis(axis_ls = axis_ls_marks, add = TRUE)

      ## Add labelled axes for plots on the left/bottom of the multi-panel figure
      # Define parameters/helper function
      mat <- matrix(1:c(mf[1] * mf[2]), nrow = mf[1], ncol = mf[2], byrow = by_row)
      bottom <- mat[nrow(mat), ]
      left   <- mat[, 1]
      top    <- mat[1, ]
      right  <- mat[, ncol(mat)]
      update_side_param <- function(side_param){
        lim        <- side_param$lim
        axis_param <- side_param$axis
        at <- axis_param$at
        if(length(at) > 1){
          axis_param <- lapply(axis_param, function(elm){
            if(length(elm) == length(at)) {
              if(at[1] == lim[1]) elm <- elm[2:length(elm)]
              if(at[length(at)] == lim[2]) elm <- elm[1:(length(elm) - 1)]
            }
            return(elm)
          })
        }
      }
      # Add axis and titles
      if(1 %in% pretty_axis_args$side){
        axis_param_x <- update_side_param(axis_ls[[1]])
        if(i %in% bottom) do.call(graphics::axis, axis_param_x)
      }
      if(2 %in% pretty_axis_args$side){
        axis_param_y <- update_side_param(axis_ls[[2]])
        if(i %in% left) do.call(graphics::axis, axis_param_y)
      }
      if(3 %in% pretty_axis_args$side){
        axis_param_x <- update_side_param(axis_ls[[3]])
        if(i %in% top) do.call(graphics::axis, axis_param_x)
      }
      if(4 %in% pretty_axis_args$side){
        axis_param_y <- update_side_param(axis_ls[[4]])
        if(i %in% right) do.call(graphics::axis, axis_param_y)
      }
      ## Add titles
      print(d$group[1])
      if(!is.null(add_main)){
        if(is.null(add_main$side)) add_main$side <- 3
        if(is.null(add_main$text)) {
          if(length(n_groups) <= 26) {
            add_main$text <-
              bquote(bold(.(LETTERS[i])) ~ "(" * .(as.character(d$group[1])) * ")")
          } else {
            add_main$text <-
              bquote(bold(.(i)) ~ "(" * .(as.character(d$group[1])) * ")")
          }
        }
        do.call(graphics::mtext, add_main)
      }
    }) %>% invisible()

    #### Add (global) axis titles
    if(!is.null(add_xlab)){
      if(is.null(add_xlab$side)) add_xlab$side <- 1
      if(is.null(add_xlab$text)) add_xlab$text <- "x"
      if(is.null(add_xlab$outer)) add_xlab$outer <- TRUE
      do.call(graphics::mtext, add_xlab)
    }
    if(!is.null(add_ylab)){
      if(is.null(add_ylab$side)) add_ylab$side <- 2
      if(is.null(add_ylab$text)) add_ylab$text <- "y"
      if(is.null(add_ylab$outer)) add_ylab$outer <- TRUE
      do.call(graphics::mtext, add_ylab)
    }

    #### Close and return
    graphics::par(pp)
    return(invisible())
  }
