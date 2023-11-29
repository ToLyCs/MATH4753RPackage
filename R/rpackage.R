#' Title calculate 95 confidence interval for mean
#'
#' @param x a sample
#'
#' @return  confidence interval for mean
#' @export
#' @importFrom stats sd  qt
#'
#' @examples
myci <- function(x) {
  # get alpha
  alpha <- 0.05
  # get n
  n <- length(x)
  # get t
  t<-qt(1-alpha/2,n-1)
  # get +- t
  mp<-c(-1,1)
  # calc ci
  ci <- mean(x)+mp*t*sd(x)/sqrt(n)
  return(ci)
}

logbin2=function(theta){log(dbinom(3,prob=theta,size=6)) + log(dbinom(5,prob=theta,size=10))}

#' to find the Max Likelihood
#'
#' @param lfun logbin2 function
#' @param theta A vector of parameter value
#' @importFrom graphics abline
#' @importFrom stats dbinom
#' @importFrom graphics axis
#' @return The parameter coresponding to the maximum likelihood
#' @export
#'
#' @examples
mymaxlikg=function(lfun="logbin2",theta) { # default log lik is a combination bin
  nth=length(theta)  # nu. of valuse used in theta
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE) # Matrix of theta
  z=apply(thmat,1,lfun) # z holds the log lik values
  zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}



#' @title Binomial Simulation
#'
#' @description Shows how to simulate a Binomial
#'
#' @details More details about the function
#'
#' @param iter Number of iterations
#' @param n Number of Bernoulli trials
#' @param p Probability of a success
#'
#' @importFrom grDevices  rainbow
#' @importFrom graphics  barplot
#'
#' @return Barplot and table of relative frequencies
#' @export
#'
#' @examples
#' mybin() # Use default values
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=vector(mode="numeric", length=iter)
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}


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

