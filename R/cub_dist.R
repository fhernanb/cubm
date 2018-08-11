#' cub distribution
#' 
#' Density, distribution function, quantile function and random generation for the cub distribution given parameters \eqn{\pi} and \eqn{\xi}.
#' 
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param pi uncertainty parameter belongs to \code{(0, 1]}.
#' @param xi feeling parameter belongs to \code{[0, 1]}.
#' @param n number of observations
#' @param m the maximum value.
#' @param log logical; if TRUE, densities are given as log.
#' @param lower.tail logical; if TRUE (default), probabilities are \code{P[X ≤ x]} otherwise, \code{P[X > x]}.
#' 
#' @examples 
#' # Examples with dcub
#' 
#' dcub(x=4, pi=0.3, xi=0.7, m=5)
#' dcub(x=1, pi=0.5, xi=0.4, m=8)
#' dcub(x=c(4, 1), pi=c(0.3, 0.5), xi=c(0.7, 0.4), m=c(5, 8))
#'
#' # Examples with pcub
#' 
#' # low xi is associated with high ratings
#' pcub(q=5, pi=0.5, xi=0.2, m=10, lower.tail=FALSE)
#' 
#' # high pi is associated with indecision in choosing
#' pcub(q=3, pi=0.9, xi=0.5, m=4) 
#' 
#' # probability for several quantiles
#' pcub(q=c(1,3,5), pi=0.3, xi=0.6, m=5)
#' 
#' # Examples with qcub
#' 
#' # low xi is associated with high ratings
#' qcub(p=0.1, pi=0.5, xi=0.1, m=7, lower.tail=TRUE)
#' 
#' # high pi is associated with indecision in choosing
#' qcub(p=0.86, pi=0.9, xi=0.5, m=4)
#' 
#' #quantiles for several probabilities 
#' qcub(p=c(1,0.5,0.9), pi=0.3, xi=0.6, m=5)
#' 
#' # Examples with rcub
#' # Random sample, low xi is associated with high ratings
#' x <- rcub(n=1000, pi=0.9, xi=0.1, m=5)
#' barplot(prop.table(table(x)))
#' 
#' # Random sample, low pi is associated with random choices
#' y <- rcub(n=1000, pi=0.1, xi=0.1, m=5)
#' barplot(prop.table(table(y)))
#' @name cub_dist
NULL
#'
#' @rdname cub_dist
#' @importFrom stats dbinom
#' @export
dcub <-function(x, pi, xi, m, log=FALSE) {
  if(any(x %% 1 != 0))
    stop(paste("x must be an integer number", "\n", ""))
  if (any(m <= 0)) 
    stop(paste("m must be positive", "\n", ""))
  if (any(pi < 0 | pi > 1)) ## Era pi <= 0
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  dens <- log(pi * dbinom(x=x-1, size=m-1, prob=1-xi) + (1 - pi) / m)
  dens[x <= 0 | x > m] <- -Inf
  if (log == FALSE) dens <- exp(dens)
  return(dens)
}
#' @rdname cub_dist
#' @export
pcub <- function(q, pi, xi, m, lower.tail=TRUE, log=FALSE) {
  if (any(m <= 1)) 
    stop("m parameter must be greater than 1", "\n", "")
  if (any(pi <= 0 | pi > 1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  # This is an auxiliar function -----------
  aux <- function(q, pi, xi, m, lower.tail=TRUE, log=FALSE) {
    val <- seq(from=-100, to=q)
    prob <- dcub(x=val, pi=pi, xi=xi, m=m)
    sum(prob)
  }
  aux <- Vectorize(aux)
  # End of auxiliar function
  p <- aux(q=q, pi=pi, xi=xi, m=m)
  if (lower.tail == FALSE) p <- 1-p
  if (log) p <- log(p)
  return(p)
}
#' @rdname cub_dist
#' @export
qcub <- function(p, pi, xi, m, lower.tail=TRUE, log=FALSE) {
  if (any(p < 0 | p > 1)) 
    stop(paste("p must be in [0,1]", "\n", ""))
  if (any(m <= 1)) 
    stop("m parameter must be greater than 1", "\n", "")
  if (any(pi <= 0 | pi > 1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  
  prob <- cumsum(apply(as.matrix(1:m, ncol=m, nrow=1), 
                       1, dcub, pi, xi, m))
  l <- sapply(p, function(x) sum(x > prob))
  l <- replace(l, l==0, 1)
  la <- cbind(prob[l], prob[l+1])
  la[is.na(la)] <- m
  med <- apply(la, 1, mean)
  med[is.na(med)] <- m 
  r <- ifelse (p < med-0.0001, l, l+1)
  if (lower.tail == TRUE)
    r
  else m-r
} 
#' @rdname cub_dist
#' @importFrom stats rbinom runif
#' @export
rcub <- function(n, pi, xi, m=5) {
  if (any(pi <= 0 | pi >1)) 
    stop(paste("pi must be in (0,1]", "\n", ""))
  if (any(xi < 0 | xi > 1)) 
    stop(paste("xi must be in [0, 1]", "\n", ""))
  if (any(m <= 1)) 
    stop("m parameter must be greater than 1", "\n", "")
  #  Define the component distributions
  rshifted.binom <- rbinom(n=n, size=m-1, prob=1-xi) + 1
  rdiscrete.unif <- sample(1:m, n, replace=T)
  mixture <- runif(n)
  r <- ifelse(mixture < pi, rshifted.binom, rdiscrete.unif)
  return(r)
}
