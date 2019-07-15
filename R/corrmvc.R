#' Correlation from a 2d covariance matrix.
#'
#' Computes the correlation given a covariance matrix of a bivariate variable.
#'
#' @param mat is a 2x2 covariace matrix
#' @return The empirical correlation fo the two series
#'
corrmvc<-function(mat){
  return(mat[1,2]/sqrt(mat[1,1]*mat[2,2]))
}
