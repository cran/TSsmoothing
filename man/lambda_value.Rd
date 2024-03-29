% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lambda_value.R
\name{lambda_value}
\alias{lambda_value}
\title{Calculation of the lambda value.}
\usage{
lambda_value(s, rho, N)
}
\arguments{
\item{s}{is a scalar that specifies the smoothing level.}

\item{rho}{is the estimated correlation of the two time series. If the time series is univariate rho should be 0.}

\item{N}{the length of the bivariate time series.}
}
\value{
The value of lambda  lambda_value that corresponds to a smoothing level s

A flag to indicate if the lambda value was read from ltable
}
\description{
Obtains the lambda value for specific values of the smoothing level, correlation and length.
}
