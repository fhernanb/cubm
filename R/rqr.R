#' Randomized Quantile Residuals for cub models.
#' 
#' This function generates Randomized Quantile Residuals.
#'
#' @param y vector with the response variable.
#' @param pi value or vector with the values for pi parameter.
#' @param xi value or vector with the values for pi parameter.
#' @param m the maximum value.
#' 
#' @examples 
#' y <- rcub(n=1000, pi=0.15, xi=0.60, m=8)
#' # To fit the model
#' mod1 <- cub(pi.fo=y~1, xi.fo=~1, m=8)
#' # To obtain the RQR
#' r1 <- rqr(y, pi=mod1$fitted.pi, xi=mod1$fitted.xi, m=8)
#' # To check the normality
#' qqplot(y=r1, x=qnorm(ppoints(1000), mean=0, sd=1), las=1,
#'        ylab='RQR', xlab='Theoretical Quantiles N(0, 1)')
#' abline(a=0, b=1, col='red')
#' # Using a test to check if rqr follow a N(0, 1)
#' ks.test(r1, "pnorm", mean=0, sd=1)
#' 
#' @importFrom stats runif
#' @export
rqr <- function(y, pi, xi, m) {
  a <- pcub(q=y-1, pi, xi, m)
  b <- pcub(q=y  , pi, xi, m)
  u <- runif(n=length(y), min=a, max=b)
  r <- qnorm(u)
  r
}
rqr <- Vectorize(rqr)
