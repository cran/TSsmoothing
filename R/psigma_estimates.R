#' Preliminar estimates
#'
#' It computes the preliminary estimates of Sigma_epsilon and Sigma_eta
#'
#' @param dat is a 2 column matrix with for the bivariate time series observations. Each column correspond to the values at a given time.
#'
#' @return Sigma epsilon
#' @return Sigma eta
#'
psigma_estimates<-function(dat){
  N=dim(dat)[1]
  gamma2<-sigma_zf(2,diff(dat[,1],lag=1,diff=2),diff(dat[,2],lag=1,diff=2),N)
  gamma1<-sigma_zf(1,diff(dat[,1],lag=1,diff=2),diff(dat[,2],lag=1,diff=2),N)
  gamma0<-sigma_zf(0,diff(dat[,1],lag=1,diff=2),diff(dat[,2],lag=1,diff=2),N)
  Sigma.epsilon<-gamma0-3/17*(gamma2+t(gamma2)-4*gamma1-4*t(gamma1)) #estim var tendencias
  Sigma.eta<-1/34*(gamma2+t(gamma2)-4*gamma1-4*t(gamma1))
  list(Sigma.epsilon=Sigma.epsilon,Sigma.eta=Sigma.eta)
}



