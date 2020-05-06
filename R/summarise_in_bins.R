#' @title Summarise continuous data in bins
#' @description This function implements a common method for describing patterns in continuous data: summary statistics in bins. One variable (\code{x} is binned into user-defined bins or breaks. User-defined functions are then calculated for a second variable \code{y} within each of these bins. This can help elucidate associations between \code{y} and \code{x} during data exploration, particularly if those associations are noisy.
#'
#' @param x The x values which will be binned. Numeric/integer, POSIXct or Date objects are supported.
#' @param y The y values for which summary statistics will be calculated in each bin.
#' @param dat (optional) A dataframe containing columns 'x' and 'y' can be supplied instead of \code{x} and \code{y} above.
#' @param bin A numeric or character input which defines the bin size. This is used to define a sequence of breaks, to which each value of \code{x} can be assigned and in each of which summary statistics are calculated. If \code{x} is a number, this should be a number; if \code{x} is a time in POSIXct format, this can be a number (in seconds), a character defining a unit of time; e.g., \code{"2 mins"}; and if x is a Date, this can be a number (in days) or a character as for POSIXct objects. For Date objects, bins smaller in size than one day are nonsensical; convert \code{x} to a POSIXct object first.
#' @param breaks (optional) A numeric, POSIXct or Date vector of breaks. This can be supplied instead of \code{bin}. If supplied, summary statistics are calculated across user-supplied breaks, rather than those calculated between the range of the data (\code{x}) based on the bin size.
#' @param funs A named list of functions to evaluate.
#' @param shift A logical input which defines whether or not to shift the value of each bin by half a bin width (i.e. to the mid-point of each bin). This is beneficial for plots because summary statistics are shown for the mid-point of each bin.
#' @param to_plot A logical input which defines whether or not the summary statistics are to be plotted (in due course). If so, the function conducts some additional checks to make sure bins are within the limits of the data: if the bin size is relatively large and \code{shift = TRUE}, it is possible that most extreme bins can be shifted outside of the range of the data; any such bins are removed from the reported outputs for pretty plotting.
#' @param output A character which defines the output type. The currently implemented options are as follows. (1) \code{"list"} outputs a list, with one element for each summary statistic (each element consists of a dataframe with three columns: 'bin', the start or mid-point of each bin depending on whether or not \code{shift = TRUE}; 'stat', the value of the summary statistic in that bin; and 'fun', the function name, taken from \code{funs}). (2) \code{"data.frame"} binds the list described above into a single (long-format) dataframe.
#'
#' @returns The function returns a list or a dataframe with summary statistics for each bin, depending on the input to \code{output}.
#'
#' @examples
#'
#' #### Example 1: Numeric example
#' # Generate example data and visualise
#' set.seed(1)
#' x <- 1:100
#' y <- stats::rnorm(length(x), x * 2 - 50, 25)
#' graphics::plot(x, y)
#' # Calculate summary statistics in bins 10 units in length by supplying custom
#' # ... functions via funs. shift = TRUE shift summary statistics to the middle value of each bin
#' # ... which can be helpful for plotting.
#' summary_ls <- summarise_in_bins(x = x,
#'                                 y = y,
#'                                 bin = 10,
#'                                 funs = list(mean = mean,
#'                                             lowerSD = function(x){mean(x) - stats::sd(x)},
#'                                             upperSD = function(x){mean(x) + stats::sd(x)}),
#'                                 shift = TRUE)
#' # Examine
#' str(summary_ls)
#' # Add a single summary statistic to the plot as a line:
#' add_lines(summary_ls$mean$bin, summary_ls$mean$stat, col = "orange", lwd = 5)
#' # Or loop over every element in the list and add a line marking the values
#' # ... of that summary statistic in a particular colour:
#' mapply(FUN = function(summary_df, col){
#'   lines(summary_df$bin, summary_df$stat,col = col, type = "b")},
#'   summary_ls,
#'   c("blue", "red", "red"))
#'
#' #### Example (3): POSIXct example
#' # Generate some example data and visualise:
#' x <- seq.POSIXt(as.POSIXct("2016-01-01", tz = "UTC"),
#'                 as.POSIXct("2016-01-05", tz = "UTC"), by = "60 mins")
#' y <- stats::rnorm(length(x), as.numeric(x) * 1e-3, 1e2)
#' graphics::plot(x, y)
#' # Calculate example summary statistics
#' summary_ls <- summarise_in_bins(x = x,
#'                                 y = y,
#'                                 bin = "days",
#'                                 funs = list(mean = mean,
#'                                             median = median),
#'                                 shift = TRUE,
#'                                to_plot = TRUE)
#' # Visualise
#' add_lines(summary_ls$mean$bin, summary_ls$mean$stat, col = "red", type = "b", lwd = 2)
#'
#' #### Example (4): Date example
#' x <- seq.Date(as.Date("2016-01-01"), as.Date("2016-10-01"), "days")
#' y <- stats::rnorm(length(x), as.numeric(x)*1e-3)
#' graphics::plot(x, y)
#' summary_ls <- summarise_in_bins(x = x,
#'                                 y = y,
#'                                 bin = "14 days",
#'                                 funs = list(mean = mean,
#'                                             median = median),
#'                                 shift = TRUE,
#'                                 to_plot = TRUE)
#' # Visualise
#' add_lines(summary_ls$mean$bin, summary_ls$mean$stat, col = "red", type = "b", lwd = 2)
#'
#' @author Edward Lavender
#' @export
#'

#######################################
#######################################
#### summarise_in_bins()

summarise_in_bins <-
  function(x,
           y,
           dat = NULL,
           bin = NULL,
           breaks = NULL,
           funs = list(mean = mean),
           shift = TRUE,
           to_plot = TRUE,
           output = "list"){

    #### Initial checks
    if(length(funs) == 0) stop("'funs' should not be an empty list.")
    if(is.null(names(funs))) warning("'funs' is not a named list.")
    if(is.null(bin) & is.null(breaks)) stop("'bin' and 'breaks' are both NULL; one of these must be specified.")
    if(!is.null(bin) & !is.null(breaks)) warning("Both 'bin' and 'breaks' specified; input to 'bin' is ignored.")

    #### Define dataframe
    if(is.null(dat)){
      dat <- data.frame(x = x, y = y)
    }

    #### Define breaks if necessary
    if(is.null(breaks)){
      seq.f <- function(x){
        if(class(x)[1] %in% c("numeric", "integer")){
          return(seq)
        } else if(class(x)[1] %in% c("POSIXct", "POSIXlt")){
          return(seq.POSIXt)
        } else if(class(x)[1] == "Date"){
          return(seq.Date)
        } else stop("class(x) is not supported: only numeric, integer, Date or date-time objects are supported.")
      }
      seq.f <- seq.f(dat$x)
      breaks <- seq.f(from = min(dat$x, na.rm = TRUE), to = max(dat$x, na.rm = TRUE), by = bin)
    }

    #### Define dat$bin
    dat$bin <- findInterval(dat$x, breaks)
    dat$bin <- breaks[dat$bin]

    #### Define a dataframe in which we'll store summary statistics for each bin
    summary_df <- data.frame(bin = sort(unique(dat$bin)))

    #### Shift summarises by half a bin if requested
    if(shift){
      if(class(x)[1] %in% c("numeric", "integer")){
        bin_numeric <- bin
      } else if(class(x)[1] %in% c("POSIXct", "POSIXlt")){
        s <- seq.POSIXt(from = breaks[1], to = breaks[2], length.out = 2)
        bin_numeric <- as.numeric(difftime(s[2], s[1], units = "secs"))
      } else if(class(x)[1] == "Date"){
        s <- seq.Date(from = breaks[1], to = breaks[2], length.out = 2)
        bin_numeric <- as.numeric(difftime(s[2], s[1], units = "days"))
        if(bin_numeric < 1){
          stop("For class(x) == Date, the bin size/gap between sequential breaks cannot be less than one day. Increase the bins size/gap between sequential breaks or convert x to a date-time object before implementing summarise_in_bins().")
        }
      }
      bin_half <- bin_numeric/2
      dat$bin <- dat$bin + bin_half
      summary_df$bin <- summary_df$bin + bin_half
    }

    #### Process dat/summarises to remove bins beyond the range of the data
    if(to_plot){
      pos_below <- which(dat$bin < min(dat$x, na.rm = TRUE))
      pos_above <- which(dat$bin > max(dat$x, na.rm = TRUE))
      pos_out <- c(pos_below, pos_above)
      if(length(pos_out) > 0){
        # Add a temporary column which we'll remove below
        summary_df$tmp <- NA
        bins2rem <- dat$bin[c(pos_out)]
        dat <- dat[which(!(dat$bin %in% bins2rem)), ]
        summary_df <- summary_df[which(!(summary_df$bin %in% bins2rem)), ]
        # A temporary column is required because otherwise the removal of bins from summary_df
        # ... causes it to collapse from a dataframe, which we don't want. Adding then removing
        # ... the extra column forces this behaviour not to occur.
        summary_df$tmp <- NULL
      }
    }

    #### Define summarises
    # Define a list of summary statistics, with one element for each function
    summary_ls <- lapply(funs, function(f){
      summary_df_tmp <- summary_df
      tbl <- tapply(dat$y, dat$bin, f)
      summary_df_tmp$stat <- as.numeric(tbl)
      return(summary_df_tmp)
    })
    names(summary_ls) <- names(funs)
    # Add statistic names to the dataframes
    for(i in 1:length(funs)){
      nam <- names(funs)[i]
      summary_ls[[i]]$fun <- nam
    }

    #### Define outputs
    if(output == "list"){
      return(summary_ls)
    } else if(output == "data.frame"){
      summary_df <- do.call("rbind", summary_ls)
      return(summary_df)
    } else{
      warning("Output format not recognised. A list is returned.")
      return(summary_ls)
    }

  } # close function


#### End of function.
##################################################
##################################################
