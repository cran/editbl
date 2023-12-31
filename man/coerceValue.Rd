% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{coerceValue}
\alias{coerceValue}
\title{\code{DT::coerceValue} with better \code{POSIXct} support}
\usage{
coerceValue(val, old)
}
\arguments{
\item{val}{A character string.}

\item{old}{An old value, whose type is the target type of \code{val}.}
}
\description{
\code{DT::coerceValue} with better \code{POSIXct} support
}
\details{
Will assume UTC in case no timezone is specified.
}
\author{
Jasper Schelfhout
}
