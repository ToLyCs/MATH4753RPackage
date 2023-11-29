#' @title This function plots the normal density curve and calculates the probability of X <= a.
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param x  Additional x-value
#' @param a Alpha value
#'
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @return A list containing the plot and the probability P(X <= a).
#' @export
#'
#' @examples myncurve(mu = 10, sigma = 5, a = 6)
myncurve = function(mu, sigma, a, x){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma,a,length=1000)
  # Y values corresponding t0 the x values
  ycurve = dnorm(xcurve, mean = mu, sd = sigma)
  # Fill in the polygon with the given vertices
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Green3")
  # Put in the text with the appropriate area
  # Area
  area=pnorm(a,mean=mu,sd=sigma)
  area=round(area,4)
  # Click to paste the text onto the graph
  text(x=2,y=0.025, paste("Area = ", area, sep=""))
  list(prob = area)
}
