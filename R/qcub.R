#' Quantile function for cub model
#'
#' This function gives the quantile distribution of a cub model without covariates given the parameters \eqn{\pi} and \eqn{\xi}.
#' 
#' @param p vector of probabilities.
#' @param pi uncertainty parameter belongs to \code{(0, 1]}
#' @param xi feeling parameter belongs to \code{[0, 1]}
#' @param m the maximum value for random values
#' @param shift the minimum value for random values
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}.
#' @param log logical; if TRUE, densities are given as log.
#' 
#' @examples 
#' 
#' # low xi is associated with high ratings
#' qcub(p=0.1, pi=0.5, xi=0.1, m=7, lower.tail=T)
#' 
#' # high pi is associated with indecision in choosing
#' qcub(p=0.86, pi=0.9, xi=0.5, m=4)
#' 
#' #quantiles for several probabilities 
#' qcub(p=c(1,0.5,0.9), pi=0.3, xi=0.6, m=5)
#' 
#' @export
#' 
qcub <- function(p, pi, xi, m, shift=1, lower.tail=TRUE, log=FALSE)
{
  if (any(p < 0 | p > 1)) 
    stop(paste("p must be in [0,1]", "\n", ""))
  if (any(m <= shift)) 
    stop("m parameter must be greater than shift", "\n", "")
  if (any(pi <= 0 | pi > 1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))

  prob <- cumsum(apply(as.matrix(1:m, ncol = m, nrow = 1), 
                       1, dcub, pi, xi, m))
  l <- sapply(p, function(x) sum(x > prob))
  l <- replace(l, l==0, 1)
  lm<- cbind(prob[l], prob[l+1])
  lm[is.na(lm)] <- m
  med <- apply(lm, 1, mean)
  med[is.na(med)] <- m 
  r <- ifelse (p < med-0.0001, l, l+1)
  if (lower.tail == TRUE)
    r
  else m-r
} 

