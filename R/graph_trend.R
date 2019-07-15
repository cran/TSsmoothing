#' Plot of original and smoothed time series.
#'
#' It plots the univariate or bivariate. This function is not intended for users but to be called by trend_estimate.
#'
#' @param dat is a 2x2 covariace matrix
#' @param N the number of observations
#' @param tau the smoothed time series
#' @param dvar the estimated variance for tau
#' @param label the vectors of caracters associated to the time points to appear in the axis
#' @param jump if label is too long, jump thin them on the axis
#' @param bands is TRUE to draw the approximately 95\%  confidence bands around tau
#' @param las is 1 and 2 if the asis should appear vertical and horizontal, respectively
#' @param bivariate is FALSE if dat is a univariate time series
#'
#'
#' @return The empirical correlation fo the two series
#'
graph_trend <- function(dat, N, tau, dvar, label=NULL, jump=1:N, bands=TRUE, las, bivariate=TRUE){
  if(is.null(label)) label <- 1:N
  if(bivariate){
    nf <-layout(matrix(1:2,2,1), heights = c(1,1), TRUE)
    opar <- par(mar = c(0, 3, 2, 3))
    on.exit(par(opar))
    plot(dat[,1], t = "l",xaxt = "n", yaxt = "n", cex.axis=.7, las = 1, lwd = 2, col = rgb(0,0.7,0.7,1), bty = "n")
    axis(4, las = 1,cex.axis = 0.7)
    grid(nx = NULL,ny = NULL,col = grey(.6))
    lines(as.numeric(time(dat)), tau[,1],t="l");
    if(bands){
      lines(as.numeric(time(dat)),tau[,1]+2*sqrt(dvar[,1]), lty = 2)
      lines(as.numeric(time(dat)),tau[,1]-2*sqrt(dvar[,1]), lty = 2)
    }
    if(is.null(jump)) jump <- round(seq(1,N,length=min(N,50)))
    par(mar = c(3, 3, 0, 3))
    plot(dat[,2], t = "l", xaxt = "n", yaxt="n", cex.axis = .7, las = 1, lwd = 2, col = rgb(0,0.7,0.7,1), bty = "n")
    axis(2, las = 1,cex.axis = 0.7)
    axis(1, at =jump, labels = label[jump], las = las, cex.axis = .6)
    grid(nx = NULL, ny = NULL, col = grey(.6))
    lines(as.numeric(time(dat)), tau[,2], lwd=1);
    if(bands){
      lines(as.numeric(time(dat)), tau[,2]+2*sqrt(dvar[,2]), lty = 2)
      lines(as.numeric(time(dat)), tau[,2]-2*sqrt(dvar[,2]), lty = 2)
    }
  }else{ #only one serie
    layout(matrix(1),heights = 1)
    if(is.null(jump)) jump <- seq(1,N,length=min(N,50))
    par(mar = c(4, 2, 2, 2))
    plot(as.numeric(dat), t = "l", xaxt = "n", cex.axis = .7,las = las, lwd = 2, col = rgb(0,0.7,0.7,1), bty = "n",xlab = "")
    axis(1, at = jump, labels = label[jump], las = las, cex.axis = .6)
    grid(nx = NULL, ny = NULL, col = grey(.6))
    lines(1:length(dat), tau, lwd = 1);
    if(bands){
      lines(1:length(dat), tau+2*sqrt(dvar), lty = 2)
      lines(1:length(dat), tau-2*sqrt(dvar), lty = 2)
    }
  }
}
