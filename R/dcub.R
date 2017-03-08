#' Density function for cub model
#' 
#' This function gives the density for a cub model given parameters \eqn{\pi}, \eqn{\xi} and \eqn{x} value.
#' 
#' @param x vector of quantiles.
#' @param pi uncertainty parameter belongs to \code{(0, 1]}.
#' @param xi feeling parameter belongs to \code{[0, 1]}.
#' @param m the maximum value.
#' @param log logical; if TRUE, densities are given as log.
#' 
#' @examples 
#' dcub(x=4, pi=0.3, xi=0.7, m=5)
#' dcub(x=1, pi=0.5, xi=0.4, m=8)
#' dcub(x=c(4, 1), pi=c(0.3, 0.5), xi=c(0.7, 0.4), m=c(5, 8))
#' 
#' @export
#' 
dcub <-function(x, pi, xi, m, log = FALSE) {
  if (any(x <= 0))
    stop(paste("x must be positive", "\n", ""))
  if(any(x %% 1 != 0))
    stop(paste("x must be an integer number", "\n", ""))
  if (any(m <= 0)) 
    stop(paste("m must be positive", "\n", ""))
  if (any(pi <= 0 | pi > 1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  dens <- log(pi * dbinom(x=x-1, size=m-1, prob=1-xi) + (1 - pi) / m)
  if (log == FALSE) dens <- exp(dens)
  return(dens)
}