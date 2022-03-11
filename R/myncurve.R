#' myncurve
#'
#' gives a normal distribution curve and shades the area under the curve
#' up to a. It also calculates the probability of point a.
#'
#' @name MyNCurve
#'
#'
#' @param mu mu
#' @param sigma sigma
#' @param a point a


globalVariables(c("curve", "dnorm", "polygon", "pnorm", "x"))
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(a-(2*mu),a,length=10000)
  ycurve=dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(a-(2*mu),xcurve,a),c(0,ycurve,0),col="Gray")

  prob=pnorm(a, mu, sigma)
  prob=round(prob,4)
  prob
}
