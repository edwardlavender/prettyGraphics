#' @title Define time blocks (i.e. diel periods or seasons)
#' @description This function defines 'blocks' (i.e., diel periods or seasons) for each day in a time window.
#'
#' @param t1 The start time, in POSIXct format with the time zone specified.
#' @param t2 The end time, in POSIXct format with the time zone specified.
#' @param type A character input specifying the type of block to define. \code{"diel"} or \code{"season"} are currently supported. \code{"diel"} defines a dataframe with \code{date}, \code{time} and \code{level} (1, time of sunrise; 2, time of sunset). \code{"season"} defines a dataframe with \code{date}, \code{time} and \code{level} representing seasons.
#' @param type_args A named list of arguments needed to implement \code{type}. This is required for \code{type = "diel"}. A list containing "lon" and "lat" is needed; these arguments are passed to \code{\link[maptools]{sunriset}}.
#' @param to_plot A logical input defining whether or not the returned dataframe is to use as an input for plotting (see \code{\link[prettyGraphics]{add_shading_bar}}. If so, the function conducts some processing so that the times are defined exactly along the limits provided by \code{t1} and \code{t2}. This also adjusts the dataframe returned (see Value).
#' @param col A vector of colours, one for each factor that are added to the dataframe if \code{to_plot = TRUE}. If \code{col} is \code{NULL}, colours are chosen by default.
#'
#' @return The function returns a dataframe. If \code{to_plot = FALSE}, the dataframe contains 3 columns: \code{date}, \code{time} and \code{level}. If \code{type == "diel"}, levels correspond to day/night; if\code{ type == "season"}, levels correspond to the four seasons. If \code{to_plot = TRUE}, \code{x1}, \code{x2} and \code{col} are returned.
#'
#' @examples
#'
#' #### Example (1) define diel blocks between two dates
#' define_time_blocks(t1 = as.POSIXct("2016-01-01", tz = "UTC"),
#'                    t2 = as.POSIXct("2016-01-10", tz = "UTC"),
#'                    type = "diel",
#'                    type_args = list(lon = 56, lat = -5),
#'                    to_plot = FALSE)
#'
#' #### Example (2) define diel blocks between two dates to plot
#' define_time_blocks(t1 = as.POSIXct("2016-01-01", tz = "UTC"),
#'                    t2 = as.POSIXct("2016-01-10", tz = "UTC"),
#'                    type = "diel",
#'                    type_args = list(lon = 56, lat = -5),
#'                    to_plot = TRUE,
#'                    col = c("white", "dimgrey")
#'                    )
#'
#' #### Example (3) define seasonal blocks between two dates
#' define_time_blocks(t1 = as.POSIXct("2016-01-01", tz = "UTC"),
#'                    t2 = as.POSIXct("2016-01-10", tz = "UTC"),
#'                    type = "diel",
#'                    type_args = list(lon = 56, lat = -5),
#'                    to_plot = FALSE)
#'
#'
#'
#' @author Edward Lavender
#' @export
#'

###########################################
###########################################
#### define_time_blocks()

define_time_blocks <-
  function(t1 = as.POSIXct("2016-01-01", tz = "UTC"),
           t2 = as.POSIXct("2017-01-01", tz = "UTC"),
           type = "diel",
           type_args = list(),
           to_plot = TRUE,
           col = NULL
           ){

    #### Extract tz
    tz <- attributes(t1)$tzone
    if(tz == ""){stop("Please specify a timezone.")}

    #### Define a sequence of dates from t1 to t2
    # Minus one day from the first xlabel so that the night is plotted on the far left
    # ... of the graph if necessary
    if(to_plot){ start <- t1 - 60*60*24 } else{ start <- t1 }
    dates_block <- seq.POSIXt(start, t2, by = "days")

    #### diel blocks
    if(type == "diel"){

      # Check that type_args have been correctly supplied
      if(!all(c("lat", "lon") %in% names(type_args))){
        stop("'lat' and 'lon' need to be specified in 'type_args' list for type = 'diel'.")
      }

      # Define dataframe
      dat_block <- data.frame(date = sort(rep(dates_block, 2)), level = c(rep(1:2, length(dates_block))))
      dat_block$level <- factor(dat_block$level)
      # Define a  matrix of coordinates
      coords <- matrix(c(type_args$lon, type_args$lat), nrow = 1)
      # Define the positions of day and night
      pos_sunrise <- which(dat_block$level == 1)
      pos_sunset <- which(dat_block$level == 2)
      # Calculate sunrise and sunset times on each date using parameters provided
      dat_block$time = rep(t1, nrow(dat_block))
      dat_block$time[pos_sunrise] <- maptools::sunriset(coords,
                                                        dateTime = dat_block$date[pos_sunrise],
                                                        direction = "sunrise",
                                                        POSIXct.out = TRUE)[,2]

      # define sunset time on day of event
      dat_block$time[pos_sunset] <- maptools::sunriset(coords,
                                                       dateTime = dat_block$date[pos_sunset],
                                                       direction = "sunset",
                                                       POSIXct.out = TRUE)[,2]
    #### season blocks
    } else if(type == "season"){

      # Define dataframe
      dat_block <- data.frame(date = as.Date(dates_block, tz = tz))
      dat_block$time <-  as.POSIXct(dat_block$date, tz = tz)
      dat_block$season <- lunar::terrestrial.season(dat_block$date)
      dat_block$level <- factor(dat_block$season, labels = 1:length(unique(dat_block$season)))

    }

    #### Adjustments for plotting
    if(to_plot){
      # Processing
      dat_block$time[which(dat_block$time < t1)] <- t1
      dat_block$time[which(dat_block$time > t2)] <- t2
      dat_block$date <- as.Date(dat_block$time, tz = tz)
      # Define default colours appropriately if not provided:
      if(is.null(col)){
        if(type == "diel"){
          col <- c("white", "dimgrey")
        } else if(type == "season"){
          col <- grDevices::grey.colors(4)
        }
      }
      # Rename dataframe
      dat_block$col <- col[dat_block$level]
      x1 <- dat_block$time[1:(nrow(dat_block)-1)]
      x2 <- dat_block$time[2:nrow(dat_block)]
      col <- dat_block$col[1:(nrow(dat_block)-1)]
      dat_block <- data.frame(x1 = x1, x2 = x2, col = col)
      dat_block$col <- as.character(dat_block$col)
    }

    #### Return dataframe
    return(dat_block)

  } # close function


#### End of code.
###########################################
###########################################
