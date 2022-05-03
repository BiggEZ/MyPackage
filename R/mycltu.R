#' @title mycltu
#' @description Plots a random uniform distribution to show how the distribution changes with sample size.
#'
#' @param n number of different observations.
#' @param iter number of iterations.
#' @param a minimum values.
#' @param b maximum value.
#'
#' @importFrom graphics hist
#' @importFrom stats runif
#' @importFrom stats dunif
#' @importFrom graphics lines
#' @importFrom stats density
#' @export
mycltu <- function(n=10,iter=1000,a=0,b=10){
  # Random uniform theorem
  y=runif(n*iter,a,b)
  # Makes a matrix of the data from y
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  # Calculates mean of the column
  w=apply(data,2,mean)
  # stores a histogram of w into param
  param=hist(w,plot=FALSE)
  # sets yman to 1.1x the max value in the density data
  ymax=max(param$density)
  ymax=1.1*ymax
  # makes histogram
  hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
  "\n", "sample size= ",n,sep=""),xlab="Sample mean")
  # add a density curve made from the sample distribution
  lines(density(w),col="Blue",lwd=3) # add a density plot
  # Add a theoretical normal curve
  curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  # Add the density from which the samples were taken
  curve(dunif(x,a,b),add=TRUE,lwd=4)
}
