#' @title prettyGraphics: Prettier graphics made easier in R
#' @author Edward Lavender
#'
#' @description  \link{prettyGraphics} is an R package designed to make the production of prettier plots easier in R. This includes functions which implement prettier versions of many standard plots, such as scatterplots, boxplots and histograms, namely through the creation of prettier axes. Other functions facilitate data exploration (such as the addition of lines coloured by a covariate to a plot), visualisation of statistical models (such as via the addition of model predictions to plots) and plot layout.
#'
#' @section Pretty axes: \describe{
#' \item{\link{pretty_seq}}{This function defines pretty sequences, given data, limits and pretty parameters.}
#' \item{\link{pretty_axis}}{This function defines and adds pretty axes to plots.}
#' \item{\link{pi_notation}}{This function translates numeric vectors into \eqn{\pi} notation.}
#' \item{\link{sci_notation}}{This function translates the 'e' notation used by base R into scientific notation.}
#' \item{\link{add_lagging_point_zero}}{This function brings all numbers up the the same number of decimal places.}
#' \item{\link{add_grid_rect_xy}}{This function adds a rectangular grid to a plot at user-defined positions.}
#' }
#'
#' @section Data exploration: \describe{
#' \item{\link{add_lines}}{This function adds lines to a plot, possibly colouring lines by the values of a covariate.}
#' \item{\link{add_colour_bar}}{This function adds a customisable colour bar legend to a plot.}
#' \item{\link{add_shading_bar}}{This function adds blocks of shading to a plot to elucidate relationships between a response and explanatory variables, one of which is a factor.}
#' \item{\link{add_shading_quantiles}}{This functions adds shading for the quantiles of observed variation to a plot.}
#' \item{\link{add_boundary_box}}{This functions adds a boundary box around observations at specified coordinates.}
#' }
#'
#' @section Statistical inference: \describe{
#' \item{\link{summarise_in_bins}}{This function computes statistical summaries of continuous data in bins.}
#' \item{\link{list_CIs}}{This function  lists model predictions/confidence intervals from fitted values and standard errors (or similar).}
#' \item{\link{add_error_bars}}{This function adds error bars to a plot.}
#' \item{\link{add_error_envelope}}{This function adds model predictions (e.g. fitted lines, confidence intervals) to plots.}
#' \item{\link{pretty_predictions_1d}}{This function plots pretty one-dimensional predictions.}
#' \item{\link{pretty_predictions_2d}}{This function plots pretty two-dimensional predictions.}
#' \item{\link{pretty_smooth_1d}}{This function plots pretty one-dimensional smooths.}
#' \item{\link{pretty_smooth_2d}}{This function plots pretty two-dimensional smooths.}
#' }
#'
#' @section Standard plotting functions: \describe{
#' \item{\link{pretty_plot}}{This function creates prettier plots for a variety of functions.}
#' \item{\link{pretty_panel}}{This function creates prettier grouped, multi-panel, plots.}
#' \item{\link{pretty_hist}}{This function creates prettier histograms.}
#' \item{\link{pretty_boxplot}}{This function creates prettier boxplots.}
#' \item{\link{pretty_curve}}{This function evaluates and plots functions.}
#' \item{\link{pretty_mat}}{This function creates pretty matrices.}
#' \item{\link{pretty_residuals}}{This function creates prettier diagnostic residual plots.}
#' }
#'
#' @section Temporal data: \describe{
#'  \item{\link{pretty_line}}{This function creates pretty number lines and timelines.}
#'  \item{\link{define_time_blocks}}{This function defines 'blocks' (i.e., diel periods or seasons) for each day in a time window.}
#'  \item{\link{pretty_ts}}{This function creates pretty time series plots.}
#'  \item{\link{pretty_ts_mat}}{This function creates 2-dimensional plots of the within and between day variation in a time series.}
#'  \item{\link{pretty_pgram}}{This function creates processed periodogram plots (power spectra).}
#'  \item{\link{vis_ts}}{An interactive Shiny-Dashboard application for the exploration of time series data.}
#'  \item{\link{add_moons}}{This function add moons to a plot of lunar phase.}
#' }
#'
#' @section Spatial data: \describe{
#'   \item{\link{pretty_map} and \link{pretty_map_from_file_raster}}{These functions create pretty maps of spatial data (in the latter case directly from source files). They are supported by helper functions for adding spatial layers to a background map (see below).}
#'     \itemize{
#'         \item{\link{add_sp_raster}}{ This function adds rasters to a background map.}
#'         \item{\link{add_sp_poly}}{ This function adds polygons to a background map.}
#'         \item{\link{add_sp_line}}{ This function adds lines to a background map.}
#'         \item{\link{add_sp_path}}{ This function adds paths to a background map.}
#'         \item{\link{add_sp_points}}{ This function adds points to a background map.}
#'         \item{\link{add_sp_grid_ll}}{ This function adds a longitude--latitude grid over a background (projected) map.}
#'    }
#'    \item{\link{add_north_arrow}}{This function adds a North arrow to a map.}
#'   \item{\link{summarise_by_lat}}{This function summarises a \code{\link[raster]{raster}} across latitudinal bands.}
#'   \item{\link{add_profile_lat}}{This function add latitudinal profiles to a plot of a \code{\link[raster]{raster}}.}
#'
#'  \item{\link{pretty_scape_3d}}{ This function creates interactive, 3-dimensional visualisations of landscapes/seascapes and/or environmental conditions.}
#'  \item{\link{vis_scape_3d}}{ An R Shiny wrapper for \link{pretty_scape_3d}.}
#'   }
#'
#' @section Colour schemes: \describe{
#'  \item{\link{pretty_cols_brewer}}{This function facilitates the creation of pretty colour schemes.}
#'  \item{\link{pretty_cols_split_heat}}{This function generates a `split-heat' colour scheme in which values either side of a break-point are coloured differently.}
#'  }
#'
#' @section Plot layout: \describe{
#'  \item{\link{par_mf}}{This function defines a suitable plotting window for a given number of plots.}
#'  \item{\link{par_tri}}{This function defines the indices of plots along the lower or upper triangle of a square multi-panel matrix.}
#' }
#'
#' @section Tidy tables: \describe{
#'  \item{\link{tidy_numbers}}{This function tidies the number columns in a dataframe.}
#'  \item{\link{tidy_write}}{This function writes a `tidy' table to file.}
#'  }
#'
#' @docType package
#' @name prettyGraphics
NULL
