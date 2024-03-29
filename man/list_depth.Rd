% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{list_depth}
\alias{list_depth}
\title{Find the maximum depth of a list}
\source{
This function and the documentation are derived from \code{plotrix::listDepth()}. The function is defined separately in \code{\link[prettyGraphics]{prettyGraphics}} to reduce reliance on non-default packages.
}
\usage{
list_depth(x)
}
\value{
The maximum number of levels in the list.
}
\description{
Descend a list and find the maximum number of levels in a list.
}
\details{
A possibly nested list of lists is descended to determine the maximum number of levels.
}
\keyword{internal}
