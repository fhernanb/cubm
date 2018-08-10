#' Randomized Quantile Residuals for cub models.
#' 
#' This function generates Randomized Quantile Residuals.
#'
#' @param mod object of cub class.
#' 
#' @examples 
#' 
#' # Test 1
#' 
#' y <- rcub(n=1000, pi=0.1, xi=0.70, m=8)
#' mod0 <- cub(pi.fo=y ~ 1, xi.fo=~1, m=8)
#' residuos <- rq.res(mod0)
#' qqnorm(residuos, ylab='Cuantiles muestrales',
#'        xlab='Cuantiles teoricos',las=1)
#' qqline(residuos, col='red')
#' 
#' # Test 2
#' 
#' rcub.covariates <- function(n, b0, b1, g0, g1, m = 5, shift = 1) {
#' x1 <- runif(n)
#' x2 <- runif(n)
#' pi <- pnorm(b0 + b1 * x1)
#' xi <- pnorm(g0 + g1 * x2)
#' y <- rcub(n=n, pi=pi, xi=xi, m=m)
#' data.frame(y, x1, x2, xi, pi)}
#' 
#' # Generating the data
#' dataset <- rcub.covariates(n=100, b0=-1, b1=1, g0=-2, g1=1.5)
#' 
#' # Fitting the model with optim optimizer
#' fit1 <- cub(pi.fo=y ~ x1, xi.fo=~ x2, m=5, data=dataset,
#'             optimizer='optim')
#' rq.res(fit1)
#' 
# rq.res function -----------------------------------------------

#' @importFrom stats runif qnorm
#' @export
#' 
rq.res <- function(mod) {
  y <- mod$y
  
  Fy <- function(y) { #Compute the cumulative distribution function
    fd <- dcub(y, pi=mod$fitted.pi, xi=mod$fitted.xi, m=mod$M)
    Fda <- function(y, fd){
      o <- order(y)
      yo <- y[o]; fo <- fd[o]
      #Fdap <- cumsum(diff(fo))
      FF <- fo[1]
      for (i in 2:length(y)) {
        if (yo[i] == yo[i-1])
          FF <- c(FF, FF[i-1])
        else
          FF <- c(FF, FF[i-1] + fo[i])}
      FF[order(o)]}
    Fda(y, fd)
  }

  #Residuals
  residuo <- function(FF) {
      aval <- (FF-1)                       #lower quantile
      bval <- (q=FF)                       # upper quantile
      uval <- runif(length(FF), aval, bval)  # gen rand. value
      rqres <- qnorm(uval)
  }

  res <- residuo(Fy(y))
  print(res)
}
