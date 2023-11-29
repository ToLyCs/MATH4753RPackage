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
