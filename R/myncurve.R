#' @title myncurve
#' @description Gives a normal distribution curve and shades the area under the curve
#' up to a. It also calculates the probability of point a.
#'
#' @usage myncurve(mean, sd, a, color)
#'
#' @name myncurve
#'
#' @param mean Mean of normal distribution function
#' @param sd Standard deviation from mean
#' @param a End point to shade under curve
#' @param color Color of shaded area under curve
#'
#' @importFrom graphics curve polygon
#' @importFrom stats dnorm pnorm
#' @export

globalVariables("x")
myncurve = function(mean = 10, sd = 2, a = 10, color = "Gray"){
  curve(dnorm(x,mean=mean,sd=sd), xlim = c(mean-3*sd, mean + 3*sd))

  xcurve=seq(a-(2*mean),a,length=10000)
  ycurve=dnorm(xcurve, mean=mean, sd=sd)
  polygon(c(a-(2*mean),xcurve,a),c(0,ycurve,0),col=color)

  prob=pnorm(a, mean, sd)
  prob=round(prob,4)
  cat("P(Y <= ",a,") = ",prob)
}
