#' The dissimilarity index Diss for the cub model
#' 
#' This function generates the Diss coefficient, the difference between observed relative frequencies and estimated (theoretical) probabilities.
#'
#' @param mod object of cub class.
#'
#' @examples 
#'
#' # Test 1 
#' 
#' y <- rcub(n=800, pi=0.30, xi=0.80, m=6)
#' fit <- cub(pi.fo=y ~ 1, xi.fo= ~ 1, m=6, shift=1)
#' Diss(fit)
#' 
#' # Test 2
#' 
#' mod <- cub(pi.fo= global ~ gender + lage,
#'            xi.fo= ~ residenc + willingn ,
#'            m=7, data=univer)
#' Diss(mod)
#'
#' @export
#'
Diss<-function (mod)
{
  fr <- prop.table(table(mod$y))
  if (mod$p.pi == 1 && mod$p.xi == 1)
    p <- dcub(x=1:mod$M, pi=mod$fitted.pi, xi=mod$fitted.xi, m=mod$M)
  else {
    aux.mat <- cbind(pi = mod$fitted.pi, xi = mod$fitted.xi, m=mod$M)
    myfun <- function(x) dcub(x = 1:x[3], m = x[3], pi = x[1], xi = x[2])  
    PRr <- t(apply(aux.mat, 1, myfun))
    p <- colSums(PRr) / mod$n
  }
  Diss <- sum(abs(fr - p))/2
  return(Diss)
}
