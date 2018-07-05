#' cub distribution
#' 
#' Density, distribution function, quantile function and random generation for the cub distribution given parameters \eqn{\pi} and \eqn{\xi}.
#' 
#' @param x vector of quantiles.
#' @param q vector of quantiles.
#' @param p vector of probabilities.
#' @param pi uncertainty parameter belongs to \code{(0, 1]}.
#' @param xi feeling parameter belongs to \code{[0, 1]}.
#' @param n number of observations
#' @param m the maximum value.
#' @param log logical; if TRUE, densities are given as log.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X ≤ x]} otherwise, \code{P[X > x]}.
#' 
#' @examples 
#' dcub(x=4, pi=0.3, xi=0.7, m=5)
#' dcub(x=1, pi=0.5, xi=0.4, m=8)
#' dcub(x=c(4, 1), pi=c(0.3, 0.5), xi=c(0.7, 0.4), m=c(5, 8))
#' 
#' @importFrom stats dbinom
#' @export
dcub <-function(x, pi, xi, m, log = FALSE, ...) {
  if (any(x <= 0))
    stop(paste("x must be positive", "\n", ""))
  if(any(x %% 1 != 0))
    stop(paste("x must be an integer number", "\n", ""))
  if (any(m <= 0)) 
    stop(paste("m must be positive", "\n", ""))
  if (any(pi < 0 | pi > 1)) ## Era pi <= 0
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  dens <- log(pi * dbinom(x=x-1, size=m-1, prob=1-xi) + (1 - pi) / m)
  if (log == FALSE) dens <- exp(dens)
  return(dens)
}
#' @rdname dcub
#' @export
pcub <- function(q) {
  q
}
#' @rdname dcub
#' @export
qcub <- function(p) {
  p
}
#' @rdname dcub
#' @export
rcub <- function(n) {
  n
}
