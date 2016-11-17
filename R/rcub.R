#' Random samples from a cub model
#' 
#' This function generates random samples from a cub model given the parameters \eqn{\pi} and \eqn{\xi}.
#' 
#' @param n number of observations
#' @param pi uncertainty parameter belongs to \code{(0, 1]}
#' @param xi feeling parameter belongs to \code{[0, 1]}
#' @param m the maximum value for random values
#' @param shift the minimum value for random values
#' 
#' @examples 
#' # Random sample, low xi is associated with high ratings
#' x <- rcub(n=1000, pi=0.9, xi=0.1, m=5)
#' barplot(prop.table(table(x)))
#' 
#' # Random sample, low pi is associated with random choices
#' y <- rcub(n=1000, pi=0.1, xi=0.1, m=5)
#' barplot(prop.table(table(y)))
#'
#' @export
#'
rcub <- function(n, pi, xi, m = 5, shift = 1) {
  if (any(pi <= 0 | pi >1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  if (any(m <= shift)) 
    stop("m parameter must be greater than shift", "\n", "")
  #  Define the component distributions
  rshifted.binom <- rbinom(n=n, size=m-shift, prob=1-xi) + shift
  rdiscrete.unif <- sample(shift:m, n, replace=T)
  mixture <- runif(n)
  r <- ifelse(mixture < pi, rshifted.binom, rdiscrete.unif)
  return(r)
}
