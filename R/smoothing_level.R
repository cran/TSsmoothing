#' Smoothing value
#'
#' Function that reports the smoothing level for a given value of lambda, N and rho (=0 if univariate).
#'
#' @param lambda a nonegative number.
#' @param rho the correlation of the time series.
#' @param N the length of the observations.
#'
#'
#' @return S
#'
#'
smoothing_level<-function(lambda, rho, N){
  diags <- list(c(1,5,rep(6,N-4),5,1), c(-2,rep(-4,N-3),-2), rep(1,N-2))
  tk2_k2<-bandSparse(N, k = -c(0:2), diagonals = diags, symmetric = TRUE) #t(k2)%*%k2
  M <- diag(N)+lambda*tk2_k2
  if(N<100){
    A <- M-lambda^2*rho^2*tk2_k2%*%(solve(M))%*%tk2_k2
    r<-1-sum(diag(solve(A)))/N
  }else{
    A <- M-lambda^2*rho^2*tk2_k2%*%(chol2inv(chol(M)))%*%tk2_k2
    r<-1-sum(diag(chol2inv(chol(A))))/N
  }
  return(s_level=r)
}
