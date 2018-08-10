#' I coefficient for cub model without covariables
#' 
#' This function generates I coefficient.
#'
#' @param mod object of cub class.
#'
#' @examples 
#' 
#' # Test 1 
#' 
#' # Generating a random sample given the values of pi and xi
#' 
#' y <- rcub(n=1000, pi=0.2, xi=0.70, m=5)
#' mod <- cub(pi.fo=y ~ 1, xi.fo=~ 1, m=5)
#' icub(mod)
#' 
#' y <- rcub(n=100, pi=0.9, xi=0.5, m=10)
#' mod1 <- cub(pi.fo=y ~ 1, xi.fo=~ 1, m=10)
#' icub(mod1)
#'
#' @export
#' 
icub <- function(mod) {
  nr <- table(mod$y)
  n <- length(mod$y)
  lsat<- -n*log(n) + sum(nr*log(nr))
  lv <- mod$objective 
  lv <- - abs(lv)
  lo <- - n*log(mod$M)
  I <- (lv - lo)/(lsat - lo)
  return(I)
}

