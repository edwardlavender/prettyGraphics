% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_tables.R
\name{tidy_numbers}
\alias{tidy_numbers}
\title{Tidy number columns in a dataframe}
\usage{
tidy_numbers(.data, digits, ignore = TRUE)
}
\arguments{
\item{.data}{A \code{\link[base]{data.frame}} or a \code{\link[dplyr]{tibble}}.}

\item{digits}{An integer or a vector of integers that define(s) the number of digits for numerical columns. If one integer is supplied, this is applied to all number columns; if a vector is supplied, vector elements are applied to number columns in the order given.}

\item{ignore}{A logical value that defines whether or not to ignore the input to \code{digits} for any number columns that only comprise integers (see \code{\link[prettyGraphics]{add_lagging_point_zero}}).}
}
\value{
The function returns a \code{\link[base]{data.frame}} or a \code{\link[dplyr]{tibble}}.
}
\description{
This function `tidies' a dataframe's number columns by representing numbers as characters and equalising the number of digits. This is designed to support the inclusion of tables created in R in word processors.
}
\details{
This function does not currently support scientific notation.
}
\examples{
# Define an example dataframe
d <- data.frame(x = 1:3, y = c(0.1, 0, 1), z = c("a", "b", NA))
# Use three decimal places for all number columns
tidy_numbers(d, 3, ignore  = FALSE)
# Use three decimal places for all number columns with decimal places
tidy_numbers(d, 3)
# Specify the number of decimal places for each number  column
tidy_numbers(d, c(2, 3))
}
\author{
Edward Lavender
}
