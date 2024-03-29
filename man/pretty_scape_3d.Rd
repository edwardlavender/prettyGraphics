% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pretty_scape_3d.R
\name{pretty_scape_3d}
\alias{pretty_scape_3d}
\title{Interactive 3d plots of landscapes or seascapes using \code{\link[plotly]{plot_ly}}}
\usage{
pretty_scape_3d(
  r,
  aggregate = NULL,
  add_surface = list(colors = grDevices::heat.colors(100)),
  add_markers = NULL,
  thin_markers = FALSE,
  buffer = NULL,
  add_paths = NULL,
  coastline = NULL,
  coastline_paths = list(line = list(color = grDevices::rgb(0, 0, 0), width = 4)),
  plane = NULL,
  plane_surface = list(showscale = FALSE),
  xlim = NULL,
  ylim = NULL,
  zlim = NULL,
  add_tick_text = TRUE,
  font = list(family = "sans", size = 18, color = "black"),
  xtitle = "",
  ytitle = "",
  ztitle = "",
  stretch = 1,
  aspectmode = "cube",
  eye = list(),
  verbose = TRUE,
  ...
)
}
\arguments{
\item{r}{A \code{\link[raster]{raster}} to be plotted. The raster should be less than, approximately, 1800 x 1800 cells (otherwise, \code{\link[plotly]{plot_ly}} will not show the plot). Raster aggregation can be implemented within the function (see \code{aggregate}, below), although this may be slow for large rasters. Interactive plots of lower resolution rasters are more responsive.}

\item{aggregate}{(optional) A named list of arguments that is passed to \code{\link[raster]{aggregate}} to aggregate raster cells. If provided, the raster, \code{r}, is aggregated within the function prior to plotting. For large rasters, this is necessary to reduce raster dimension to a size which can be handled by \code{\link[plotly]{plot_ly}}, which creates the underlying plot. The raster can also be reduced in size by zooming around points provided (see \code{buffer}).}

\item{add_surface}{(optional) A named list of arguments that is passed to \code{\link[plotly]{add_surface}} to customise the raster surface added to the plot.}

\item{add_markers}{(optional) A named list of arguments that is passed to \code{\link[plotly]{add_markers}} to add points to the plot.}

\item{thin_markers}{(optional) A logical input which defines whether or not plot all inputted markers (\code{thin_markers = FALSE}) or only those markers that fall within the domain of the raster (\code{thin_markers = TRUE}).}

\item{buffer}{(optional) A named list of arguments that is passed to \code{\link[rgeos]{gBuffer}} to add a buffer around inputted points,  provided via \code{add_markers}, below (e.g. \code{buffer = list(width = 1)}). If provided, only the raster cells which fall into this/these buffer(s) are shown.}

\item{add_paths}{(optional) A named list of arguments that is passed to \code{\link[plotly]{add_paths}} to add lines to the plot.}

\item{coastline}{(optional) A \code{\link[sp]{SpatialPointsDataFrame-class}} object which defines the coastline (if applicable). This option is probably only suitable if \code{aspectmode = "data"} (see below).}

\item{coastline_paths}{A named list of arguments that is passed to \code{\link[plotly]{add_paths}} to add the coastline as lines to the plot.}

\item{plane}{(optional) A number which defines the height of a horizontal, 2 dimensional plane that can be added to the plot to aid inference. Note that if \code{stretch} is provided (see below), the height of the plane is also adjusted by \code{stretch} internally.}

\item{plane_surface}{(optional) A named list of arguments that is passed to \code{\link[plotly]{add_surface}} to customise the plane added to the plot.}

\item{xlim}{(optional) A numeric vector of length 2 which defines x axis limits. If not provided, these are calculated internally.}

\item{ylim}{(optional) A numeric vector of length 2 which defines y axis limits. If not provided, these are calculated internally.}

\item{zlim}{(optional) A numeric vector of length 2 which defines z axis limits. If not provided, these are calculated internally.}

\item{add_tick_text}{A logical input that defines whether or not to add tick mark labels.}

\item{font}{A named list of arguments that control plot font (see \code{\link[plotly]{layout}}).}

\item{xtitle}{(optional) A character string which defines the label for the x axis.}

\item{ytitle}{(optional) A character string which defines the label for the y axis.}

\item{ztitle}{(optional) A character string which defines the label for the z axis.}

\item{stretch}{(optional) A number which is used to vertically stretch the height of a landscape. This can be useful if \code{aspectmode = "data"} but the magnitude of change horizontally versus vertically is very different. If supplied, \code{stretch} also affects the z values of the markers and paths (if \code{add_markers} and/or \code{add_paths} is/are specified).}

\item{aspectmode}{A character which defines the shape of the plot: \code{"cube"} produces a cube; \code{"data"} provides a plot whether the size of the x, y and z axes is scaled according to the data.}

\item{eye}{(optional) A named list of arguments that control the camera perspective (see \code{\link[plotly]{layout}}).}

\item{verbose}{A logical input which defines whether or not to display messages regarding function progress. This can be useful if \code{aggregate} or \code{buffer} are provided: for large rasters and/or many points, these steps may be slow.}

\item{...}{Additional arguments passed to \code{\link[plotly]{plot_ly}}.}
}
\value{
The function returns a \code{\link[plotly]{plotly}} plot.
}
\description{
This is a wrapper function for \code{\link[plotly]{plot_ly}} which streamlines the production of 3d, interactive raster plots. Key features include (a) automated handling of raster orientation and appropriate labelling, (b) internal aggregation of large rasters, if requested, (c) zooming around inputted points, if requested, (d) the addition of coastlines to 3d rasters, if requested, and (e) additional control over plot shape. Points (e.g. passive acoustic telemetry receivers) and lines (e.g. movement pathways) can be added across the landscape. Some other \code{\link[plotly]{plot_ly}} options are also supported. This function was motivated by the need to visualise rapidly a complex bathymetric landscape with passive acoustic telemetry receivers and reconstructed animal movement pathways over the seabed.
}
\details{
The raster dimension should be less than approximately 1800 cells by 1800 cells. The coordinate system for the raster and, if applicable, markers and the buffer width, all need to be identical, whether this is implicit or specified explicitly.
}
\examples{
#### Define some bathymetry data
library(raster)
r <- dat_gebco
r[r[] > 0] <- 0

#### Example 1: Plot a landscape using default options
pretty_scape_3d(r = r)

#### Example 2: Adjusting axes via zlim, ylim, zlim and font arguments
pretty_scape_3d(r = r,
                xlim = c(-5.5, -5.3),
                font = list("Times"))

#### Example 3: Change the aspectmode to "data":
# The x, y and z values should be in the same units.
# Here, depth is in m, so we'll project the raster to UTM coordinates:
r <- raster::projectRaster(r, crs = sp::CRS("+proj=utm +zone=29 ellps=WGS84"))
pretty_scape_3d(r = r, aspectmode = "data")
# For large areas, it can be helpful to vertically stretch the raster
pretty_scape_3d(r = r, stretch = 50, aspectmode = "data")

#### Example 4: Aggregrate the raster via aggregate
# This is necessary for large rasters for plotly to produce the plot.
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                aggregate = list(fact = 2, fun = mean))

#### Example 5: Add points via add_markers and zoom into landscape around points
# Add points
xyz <- matrix(raster::coordinates(r)[500:520, ], ncol = 2)
xyz <- data.frame(x = xyz[, 1], y = xyz[, 2], z = -500)
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                add_markers = list(x = xyz$x, xyz$y, xyz$z))
# Control point characteristics
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                add_markers = list(x = xyz$x, y = xyz$y, z = xyz$z,
                                   marker = list(color = "red")))
# Zoom into landscape around points via buffer argument
# Note that the units are in m here (since we're using the UTM coordinate system)
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                add_markers = list(x = xyz$x, y = xyz$y, z = xyz$z,
                                   marker = list(color = "red")),
                buffer = list(width = 1500))

#### Example 6: Add lines via add lines
# Note that the legend for these lines is hidden automatically
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                add_paths = list(x = xyz$x, y = xyz$y, z = xyz$z))

#### Example 6: Incorporate coastline via add_coastline and coastline_paths
# r and coastline need to have same crs
coastline <- sp::spTransform(dat_coast_around_oban, raster::crs(r))
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                coastline = coastline)
# Control coastline graphical parameters
# Note that the legend is hidden automatically
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                coastline = coastline,
                coastline_paths = list(line = list(color = "red")))

#### Example 7: Add plane
# Note that the plane height is automatically adjusted by stretch
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                plane = -50)
# Customise via plane_surface
# Note that the legend is hidden automatically
pretty_scape_3d(r = r,  stretch = 50, aspectmode = "data",
                plane = -50, plane_surface = list(colorscale = "blue", showscale = FALSE))

}
\author{
Edward Lavender
}
