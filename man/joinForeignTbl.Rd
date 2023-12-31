% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/foreignTbl.R
\name{joinForeignTbl}
\alias{joinForeignTbl}
\title{Merge a tbl with it a foreignTbl}
\usage{
joinForeignTbl(
  tbl,
  foreignTbl,
  keepNA = TRUE,
  by = foreignTbl$by,
  copy = TRUE,
  type = c("inner", "left")[1]
)
}
\arguments{
\item{tbl}{\code{tbl}}

\item{foreignTbl}{\code{list} as created by \code{\link{foreignTbl}}}

\item{keepNA}{\code{logical} keep rows from tbl with NA keys.}

\item{by}{named \code{character}, columns to join on.}

\item{copy}{\code{logical}, whether or not to copy the \code{foreignTbl} to the source of argument \code{tbl} for joining.}

\item{type}{\code{character(1)}, type of joint to perform. Can be 'inner' or 'left'.}
}
\value{
\code{tbl}, containing both columns from argument \code{tbl} and argument \code{foreignTbl}.
}
\description{
Merge a tbl with it a foreignTbl
}
\details{
see also \code{dplyr} join functions, for example \code{dplyr::left_join}.
}
\author{
Jasper Schelfhout
}
