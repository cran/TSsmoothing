#' Checks if a squared matrix is positive definite and turn it to positive definied if necessary
#'
#' @param m Is a 2x2 matrix.
#' @param c Is a small nonegative number.
#'
#' @return The same matrix (if positive definite) or its modification that is positive definite.
#'
positive_definite <- function(m, c=NULL){
  if(!is.null(c) && c<=0) stop("Parameter c must be a nonegative number.")
  ei <- eigen(m)
  if(any(ei$values<0)){
    cc<-10^{seq(-5,10,by=1)}
    ord<-min(abs(ei$values))/cc
    if(is.null(c)) c<-cc[max(which(ord>100))]
    neigen <- ei$values+abs(min(ei$values))+c
    m <- ei$vectors%*%diag(neigen,2,2)%*%t(ei$vectors)
  }
  return(list(m=m, c=c))
}


