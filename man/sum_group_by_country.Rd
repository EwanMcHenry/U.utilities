% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/U_utilities_functions.R
\name{sum_group_by_country}
\alias{sum_group_by_country}
\title{Sum Values by Country and Add UK Total}
\usage{
sum_group_by_country(
  df,
  of_colmn,
  by_var = NULL,
  cars.areas = NULL,
  cars.names = NULL
)
}
\arguments{
\item{df}{Data frame to process.}

\item{of_colmn}{Column name to sum.}

\item{by_var}{Optional grouping variable.}

\item{cars.areas}{Optional vector of car area values.}

\item{cars.names}{Optional vector of car names.}
}
\value{
Data frame with sums by country and UK total.
}
\description{
This function sums values by country and adds totals for England and the UK.
}
