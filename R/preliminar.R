#' Preliminar smoothing
#'
#' Obtains the preliminar smoothed series, based on the preliminar lambda value and empirical estimates for Sigma_eta and Sigma_epsilon. This function is called by trend_estimate as part of the smoothing process.
#'
#' @param dat is a 2-column matrix with the observations of a bivariate time series. Each row correspond to the values at a given time.
#'
#' @return The preliminary smoothen series ptau.
#' @return The final estimate for sigma.eta
#' @return The time series correlation given by sigma.eta, rho.eta.
#' @return The preliminary estimation for sigma_epsilon, sigma.epsilon,
#' @return A suggested value for lambda given by the empirical estimations.
#' @return The empirical time series correlation (preliminar to rho.eta), emp_rho.
#' @return The time series length N.
#'
preliminar <- function(dat){
  N <- nrow(dat)
  diags <- list(c(1,5,rep(6,N-4),5,1), c(-2,rep(-4,N-3),-2), rep(1,N-2))
  tk2_k2<-bandSparse(N, k = -c(0:2), diagonals = diags, symmetric = TRUE) #t(k2)%*%k2
  r <- psigma_estimates(dat)
  sigma.epsilon <- r$Sigma.epsilon
  sigma.epsilon <- positive_definite(sigma.epsilon)$m
  sigma.eta <- r$Sigma.eta
  sigma.eta <- positive_definite(sigma.eta)$m
  emp_rho<-corrmvc(sigma.eta)
  Lambda <- diag(diag(sigma.eta)/diag(sigma.epsilon),2,2)
  lambda <- mean(diag(Lambda))
  #preliminary tau estimate
  tau <- solve(diag(2*N)+lambda*(tk2_k2%x%diag(2)))%*%as.vector(t(dat)) #tau <- chol2inv(diag(2*N)+lambda*(tk2_k2%x%diag(2)))%*%as.vector(t(dat))
  tau <- matrix(tau,byrow = TRUE,nrow = N)
  esttau <- cbind(diff(tau[,1],lag=1,diff=2),diff(tau[,2],lag=1,diff=2))
  inv.sigma.eta <- solve(sigma.eta)
  sigma.eta11 <- (sum((dat[,1]-tau[,1])^2)*inv.sigma.eta[1,1]+lambda*sum(esttau[,1]^2))/(N-2)
  sigma.eta22 <- (sum((dat[,2]-tau[,2])^2)*inv.sigma.eta[2,2]+lambda*sum(esttau[,2]^2))/(N-2)
  sigma.eta12 <- (sum((dat[,1]-tau[,1])*(dat[,2]-tau[,2]))*inv.sigma.eta[1,2]+lambda*sum(esttau[,1]*esttau[,2]))/(N-2)
  #cat("Rho eta =  ",sigma.eta12/sqrt(sigma.eta11*sigma.eta22),"\n")
  sigma.eta <- matrix(c(sigma.eta11,sigma.eta12,sigma.eta12,sigma.eta22),2,2)
  list(ptau = tau, sigma.eta = sigma.eta, rho.eta=corrmvc(sigma.eta), sigma.epsilon = sigma.epsilon,lambda = lambda, emp_rho=emp_rho, N = N)
}
