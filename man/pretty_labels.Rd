% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pretty_axis_support.R
\name{pretty_labels}
\alias{pretty_labels}
\title{Define pretty labels}
\usage{
pretty_labels(
  x,
  at,
  n = NULL,
  pi_notation_args = NULL,
  sci_notation_args = list()
)
}
\arguments{
\item{x}{An object, such as a numeric vector.}

\item{at}{A numeric vector of axis positions.}

\item{n}{(optional) An integer which defines the number of decimal places for numeric axes. This is passed to \code{\link[prettyGraphics]{add_lagging_point_zero}}. If \code{NULL}, \code{n} is defined internally.}

\item{pi_notation_args}{A named list of arguments passed to \code{\link[prettyGraphics]{pi_notation}} (excluding \code{x}).}

\item{sci_notation_args}{A named list of arguments passed to \code{\link[prettyGraphics]{sci_notation}} (excluding \code{x}).}
}
\value{
A vector of labels, of the same length as axis tick marks (\code{at}).
}
\description{
This function defines pretty axis labels from an object.
}
\details{
For factors, factor levels at positions specified by \code{at} are taken as labels. For numbers, \code{\link[prettyGraphics]{add_lagging_point_zero}} and \code{\link[prettyGraphics]{sci_notation}} are implemented as necessary to define pretty labels.
}
\author{
Edward Lavender
}
\keyword{internal}
