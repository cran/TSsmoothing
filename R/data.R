#' Lambda values table.
#'
#' An array that presents the lambda values according to time series (N), the smoothing value (s),
#' and the ts correlation (rho).
#'
#' 3d array of float number that correspond to the lambda values that correspond to the
#' specified values of the length of the
#'
#' @format A 3d array with dimension 393 x 12 x  11, where dimensions are:
#' \describe{
#'   \item{N}{ with values from 8 to 400}
#'   \item{s}{ with smoothing values c(0.5, 0.6, 0.7, 0.75, 0.8, 0.825, 0.85, 0.875, 0.9, 0.925, 0.95, 0.975)}
#'   \item{rho}{ with values c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.95)}
#' }
#' @source \url{http://www.diamondse.info/}
"ltable"



#' Employment in agriculture
#'
#' Dataset of the % of total employment in agriculture (modeled ILO estimate) in OCDE members.
#'
#' @format A ts vector a length of 25 observations from 1991 to 2015.
#' @source \url{https://databank.worldbank.org/source/jobs#}
"emp_agr"



#' Annual Trade for USA and Mexico
#'
#' A dataset (matrix) containing the annual trade (% of GDP)  for USA and Mexico, from 1969 to 2017
#'
#' @source \url{https://databank.worldbank.org/source/world-development-indicators#}
"trade"
