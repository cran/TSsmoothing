#' Empirical cross-covarinace.
#'
#' Function that calculates the empirical cross-covariance of order h for a bivariate time series.
#'
#' @param h the lag value.
#' @param vec1  observations for the first variable of the bivariate time series.
#' @param vec2  observations for the second variable of the bivariate time series.
#' @param N the common length of vec1 and vec2.
#'
#' @return The value of lambda that corresponds to a smoothing level s.
#'
#'
sigma_zf<-function(h, vec1, vec2, N){
  sigma.z<-matrix(NA,2,2)
  if(h>0){
    sigma.z[1,1] <- sum(vec1[-(1:h)]*vec1[1:(N-2-h)])/(N-2-h)
    sigma.z[2,2] <- sum(vec2[-(1:h)]*vec2[1:(N-2-h)])/(N-2-h)
    sigma.z[1,2] <- sum(vec1[-(1:h)]*vec2[1:(N-2-h)])/(N-2-h)
    sigma.z[2,1] <- sum(vec2[-(1:h)]*vec1[1:(N-2-h)])/(N-2-h)
  }else{
    sigma.z[1,1] <- sum(vec1*vec1)/(N-2)
    sigma.z[2,2] <- sum(vec2*vec2)/(N-2)
    sigma.z[1,2] <- sum(vec1*vec2)/(N-2)
    sigma.z[2,1] <- sum(vec2*vec1)/(N-2)
  }
  return(sigma.z)
}



