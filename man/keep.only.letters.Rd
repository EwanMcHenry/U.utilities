% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/U_utilities_functions.R
\name{keep.only.letters}
\alias{keep.only.letters}
\title{Remove Non-Alphabetic Characters}
\usage{
keep.only.letters(x)
}
\arguments{
\item{x}{Character vector. The input string(s) from which non-alphabetic characters will be removed.}
}
\value{
Character vector. The input string(s) with all non-alphabetic characters removed.
}
\description{
This function removes all non-alphabetic characters from a string, leaving only letters (both uppercase and lowercase).
}
\examples{
\dontshow{if (requireNamespace("stringr", quietly = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
keep.only.letters("Hello, World! 123") # Returns "HelloWorld"
keep.only.letters(c("Test!@#123", "Another $tring!!")) # Returns c("Test", "Anothertring")
\dontshow{\}) # examplesIf}
}
