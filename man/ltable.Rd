% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ltable}
\alias{ltable}
\title{Lambda values table.}
\format{A 3d array with dimension 393 x 12 x  11, where dimensions are:
\describe{
  \item{N}{ with values from 8 to 400}
  \item{s}{ with smoothing values c(0.5, 0.6, 0.7, 0.75, 0.8, 0.825, 0.85, 0.875, 0.9, 0.925, 0.95, 0.975)}
  \item{rho}{ with values c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)}
}}
\source{
\url{http://www.diamondse.info/}
}
\usage{
ltable
}
\description{
An array that presents the lambda values according to time series (N), the smoothing value (s),
and the ts correlation (rho).
}
\details{
3d array of float number that correspond to the lambda values that correspond to the
specified values of the length of the
}
\keyword{datasets}
