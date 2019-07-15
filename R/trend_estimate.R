#' Trend estimation with controlled smoothing.
#'
#' This is the main function that estimates the trend for univariate or bivariate time series for a specified
#' smoothing level.
#'
#' @param dat is a 2x2 matrix with the two time series. Each column correspond to the values at a given time.
#' @param smoothing_level is a scalar between 0 and 1 that specifies the smoothing of the resulting time series tau.
#' @param lambda Alternative, the function directly accepts the lambda value that corresponds to the desired smoothing level.
#' @param plot is TRUE when we cant to plot of the original agaist the resulting series.
#' @param label vector of characters that corresponds to the labels for each time point in the serie.
#' @param jump is a vector of integers that specifies which values of labels should appear in the x labels.
#' @param las is 1(2) if the x labels should be vertical (horizontal).
#' @param bands is TRUE tolo include 95\% confidence bands in the plots.
#'
#' @return The smoothed series tau.
#' @return The orginal data dat.
#' @return The estimation for sigma_eta, sigma.eta
#' @return The length of the time series N.
#' @return The lambda value corresponding to the smoothing level.
#' @return The diagonal values of the estimated variance of tau, diag.var.tau
#' @return A flag that indicates if data is a bivariate time series.
#'
#'
#' @examples
#'
#' # Employment in agriculture (\% of total employment) (modeled ILO estimate) in OCDE members
#' data(emp_agr) #It is a ts object with one single time series
#' sts<-trend_estimate(emp_agr,0.70)
#' plot_trend(sts, title="Employment in agriculture in OCDE members", xlab = "Years")
#'
#' # Data Trade (\% of GDP) for USA and Mexico downloaded from
#' data(trade) #It is a numeric matrix with two columns
#' sts<-trend_estimate(trade,0.7)
#' plot_trend(sts, title="Trade in% of GDP",xlab="years")
#'
#' ts_trade<-ts(trade, start=1969,end=2017) #We transform tade to a ts object
#' sts<-trend_estimate(ts_trade,0.7)
#' plot_trend(sts, title="Trade in% of GDP",xlab="years")
#'
#'
trend_estimate<-function(dat, smoothing_level = NULL, lambda = NULL, plot = TRUE, label =time(dat), jump = NULL,  las = 2, bands = TRUE){
  if(is.null(smoothing_level) && is.null(lambda) ) stop("You must provide a smoothing level or a lambda value")
  if(!is.null(lambda) && lambda<0 ) stop("Lambda has to be positive")
  if(!is.null(smoothing_level) && is.null(lambda)) if(smoothing_level<0 || smoothing_level>1) stop("The Smoooth_level has to a number between 0 and 1")


  if(!is.null(ncol(dat)) && ncol(dat)==2){ #bivariate time series
    bivariate<-TRUE
    pre<-preliminar(dat)
    if(!is.null(smoothing_level)){
      lambda<-lambda_value(smoothing_level,abs(corrmvc(pre$sigma.eta)),pre$N)$lambda
      if(is.na(lambda)) stop("Select a smaller smoothing_level \n")
    }
    N <- pre$N
    diags <- list(c(1,5,rep(6,N-4),5,1), c(-2,rep(-4,N-3),-2), rep(1,N-2))
    tk2_k2 <- bandSparse(N, k = -c(0:2), diagonals = diags, symmetric = TRUE) #t(k2)%*%k2
    sigma.eta <- pre$sigma.eta
    ######################################################################
    tau <- matrix(solve(diag(2*N)+lambda*(tk2_k2%x%diag(2)))%*%as.vector(t(dat)),byrow=TRUE,nrow=N)
    #tau <- matrix(chol2inv(diag(2*N)+lambda*(tk2_k2%x%diag(2)))%*%as.vector(t(dat)),byrow=TRUE,nrow=N)

    esttau <- cbind(diff(tau[,1],lag=1,diff=2),diff(tau[,2],lag=1,diff=2))
    inv.sigma.eta <- solve(sigma.eta)
    sigma.eta11 <- (sum((dat[,1]-tau[,1])^2)*inv.sigma.eta[1,1]+lambda*sum(esttau[,1]^2))/(N-2)
    sigma.eta22 <- (sum((dat[,2]-tau[,2])^2)*inv.sigma.eta[2,2]+lambda*sum(esttau[,2]^2))/(N-2)
    sigma.eta12 <- (sum((dat[,1]-tau[,1])*(dat[,2]-tau[,2]))*inv.sigma.eta[1,2]+lambda*sum(esttau[,1]*esttau[,2]))/(N-2)
    sigma.eta<-matrix(c(sigma.eta11,sigma.eta12,sigma.eta12,sigma.eta22),2,2)
    #cat("Final Rho eta = ",corrmvc(sigma.eta),"\n")
    SS <- (lambda*sigma.eta)%*%diag(1/diag(sigma.eta),2,2)
    ######################################################################
    var.tau <- (diag(N)%x%sigma.eta)%*%solve((diag(2*N)+(tk2_k2)%x%SS))
    #var.tau <- (diag(N)%x%sigma.eta)%*%chol2inv((diag(2*N)+(tk2_k2)%x%SS))

    dvar <- matrix(diag(var.tau),byrow=TRUE,nrow=N)
    if(plot) graph_trend(dat,N,tau,dvar,label,jump,bands=bands,las,bivariate=TRUE)
    res <- list(tau=tau,dat=dat,sigma.eta=sigma.eta, N=N,lambda=lambda, diag.var.tau=dvar, bivariate = bivariate)
  }else{ #if dat is a univariate ts
    bivariate <- FALSE
    if(!is.null(smoothing_level)){
      lambda <- lambda_value(smoothing_level,rho=0,length(dat))$lambda_value
      if(is.na(lambda)) stop("Select a smaller smoothing_level \n")
    }
    N <- length(dat)
    diags <- list(c(1,5,rep(6,N-4),5,1), c(-2,rep(-4,N-3),-2), rep(1,N-2))
    tk2_k2 <- bandSparse(N, k = -c(0:2), diagonals = diags, symmetric = TRUE) #t(k2)%*%k2
    ######################################################################
    tau <- solve(diag(N)+lambda*(tk2_k2))%*%as.matrix(dat) #(5) in Guerrero 2008
    #tau <- chol2inv(diag(N)+lambda*(tk2_k2))%*%as.matrix(dat) #(5) in Guerrero 2008

    dvar <- (sum((as.matrix(dat)-tau)^2)+lambda*sum((diff(tau,lag=1,diff=2))^2))/(N-2)
    if(plot) graph_trend(dat,N,tau,dvar,label,jump,bands=bands,las,bivariate=FALSE)
    res <- list(tau = as.vector(tau), dat = dat, sigma.eta=NULL, N = N, lambda = lambda, diag.var.tau = dvar, bivariate = bivariate)
  }
res
}

