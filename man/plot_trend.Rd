% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_trend.R
\name{plot_trend}
\alias{plot_trend}
\title{Plot fo the time series and its smoothed version in ggplo}
\usage{
plot_trend(smoothedTS, title = NULL, xlab = "Time",
  ylab = names(dat)[1:2])
}
\arguments{
\item{smoothedTS}{Is an object generated by the function trend_estimate}

\item{title}{Main title of the graph}

\item{xlab}{Common x label}

\item{ylab}{A 2-length vector of characters.}
}
\value{
The ggplot of the original time series, their treind and its approximated 95% bands
}
\description{
It plots the univariate or bivariate time series and its smoothed version (trend) using ggplot. It directly uses the output of trend_estimate.
}
