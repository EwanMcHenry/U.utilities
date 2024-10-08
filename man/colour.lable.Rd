% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/U_utilities_functions.R
\name{colour.lable}
\alias{colour.lable}
\title{Generate  Color Break Labels}
\usage{
colour.lable(
  x,
  lims,
  n = 5,
  round_to = 1,
  just_pretty = T,
  transformation = "identity",
  manual_breaks = NULL
)
}
\arguments{
\item{x}{Numeric vector. The data to be used for determining if the "+" symbol should be added to the highest break.}

\item{lims}{Numeric vector of length 2. Specifies the minimum and maximum limits for the breaks.}

\item{n}{Integer. The number of breaks to generate. Default is 5.}

\item{round_to}{Integer. The number to round the breaks to (e.g., 1 for rounding to the nearest whole number). Default is 1.}

\item{just_pretty}{Logical. Whether to make the breaks look "pretty" using the \code{pretty} function. Default is TRUE.}

\item{transformation}{Character. A transformation to apply to the breaks, such as "identity", "log", or "sqrt". Default is "identity".}

\item{manual_breaks}{Option to provide manual breaks directly.}
}
\value{
A character vector of labels for the breaks, with the highest value possibly having a "+" symbol if \code{max(x)} exceeds \code{max(lims)}.
}
\description{
This function generates labeled breaks for color scales based on provided limits.
It supports appending a "+" symbol to the highest break if the data exceeds the specified limits.
}
