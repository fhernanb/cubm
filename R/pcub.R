#' Distribution function for cub model
#'
#' This function gives the probability distribution of a cub model without covariates given the parameters \eqn{\pi} and \eqn{\xi}.
#' 
#' @param q vector of quantiles.
#' @param pi uncertainty parameter belongs to \code{(0, 1]}
#' @param xi feeling parameter belongs to \code{[0, 1]}
#' @param m the maximum value for random values
#' @param shift the minimum value for random values
#' @param lower.tail logical; if TRUE (default), probabilities are \eqn{P[X \leq x]} otherwise, \eqn{P[X > x]}.
#' @param log logical; if TRUE, densities are given as log.
#' 
#' @examples 
#' # low xi is associated with high ratings
#' pcub(q=5, pi=0.5, xi=0.2, m=10, lower.tail=FALSE)
#' 
#' # high pi is associated with indecision in choosing
#' pcub(q=3, pi=0.9, xi=0.5, m=4) 
#' 
#' # probability for several quantiles
#' pcub(q=c(1,3,5), pi=0.3, xi=0.6, m=5)
#' 
#' @export
#' 
pcub <- function(q, pi, xi, m, shift = 1, lower.tail=TRUE, log=FALSE) 
{
  if(any(q %% 1 != 0))
    stop(paste("q must be an integer number", "\n", ""))
  if (any(m <= shift)) 
    stop("m parameter must be greater than shift", "\n", "")
  if (any(pi <= 0 | pi > 1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  if (any(q > m | q < 1))
    stop("q must be 1,2, ...,m", "\n", "")
  prob <- cumsum(apply(as.matrix(1:m, ncol=m, nrow=1),
                       MARGIN=1, dcub, pi, xi, m))
  p <- prob[q]
  if (lower.tail == FALSE) p <- 1-p
  if (log) p <- log(p)
  return(p)
}


