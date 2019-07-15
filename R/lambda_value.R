#' Calculation of the lambda value.
#'
#' Obtains the lambda value for specific values of the smoothing level, correlation and length.
#'
#' @param s is a scalar that specifies the smoothing level.
#' @param rho is the estimated correlation of the two time series. If the time series is univariate rho should be 0.
#' @param N the length of the bivariate time series.
#'
#'
#' @return The value of lambda  lambda_value that corresponds to a smoothing level s
#' @return A flag to indicate if the lambda value was read from ltable
#'
lambda_value <- function(s, rho, N){
  #Lambda value for the given smoothing level s
  table_used <- FALSE #flag to indicate if the table ltable is used
  #data("ltable", envir=environment())
  #data("data/ltable")
  parval <- dimnames(ltable)  #ltable is an 3d array. Its dim is n x s x rho
  if(is.element(N,as.numeric(parval[[1]])) && is.element(s,as.numeric(parval[[2]])) && is.element(rho,as.numeric(parval[[3]]))){
    res <- ltable[as.numeric(parval[[1]])==N,as.numeric(parval[[2]])==s,as.numeric(parval[[3]])==rho]
    table_used <- TRUE
  }else{
    res <- tryCatch(uniroot(function(x, s, rho, N){smoothing_level(x, rho, N)-s}, interval = c(0,1e9), rho= rho, N = N, s = s)$root,error=function(e) NA )
  }
  list(lambda_value = res, table_used = table_used)
}



