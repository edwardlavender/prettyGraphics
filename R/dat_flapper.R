#' @title A sample of flapper skate depth and temperature time series
#' @description A dataset containing a sample of flapper skate (\emph{Dipturus intermedius}) depth and temperature time series. Flapper skate are large, Critically Endangered, benthopelagic elasmobranchs. To understand the depth use of individuals within the Loch Sunart to the Sound of Jura Nature Conservation Marine Protected Area, Marine Scotland Science and Scottish Natural Heritage tagged multiple individuals with archival tags, programmed to record depth and temperature at high temporal resolution. \code{dat_flapper} is a small sample of these data including depth (m) and temperature (°C) observations every 2 minutes over a two-week period for two individuals.
#'
#' @format A dataframe with 20,161 observations and 4 variables:
#' \describe{
#'   \item{timestamp}{A POSIXct object defining the time of each observation (depth/temperature).}
#'   \item{temp}{A numeric value defining the water temperature (°C) around a given individual at a given time.}
#'   \item{depth}{A numeric value defining the depth (m) of a given individual at a given time.}
#'   \item{id}{A factor with two levels ("A" and "B") distinguishing sample time series from two different individuals.}
#' }
#'
#' @source Depth time series were collected by Marine Scotland Science and Scottish Natural Heritage.
"dat_flapper"
