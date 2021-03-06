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
#' 
#' # Example 1, without covariates -------------------------------------------
#' 
#' set.seed(456)
#' y <- rcub(n=1000, pi=0.15, xi=0.6, m=8)
#' 
#' mod1 <- cub(pi.fo=y~1, xi.fo=~1, m=8)
#' summary(mod1)
#' 
#' # Residuals for the correct model
#' r1 <- rqr(y, pi=mod1$fitted.pi, xi=mod1$fitted.xi, m=8)
#' 
#' # Residuals for a wrong model, using arbitrary estimated parameters
#' r2 <- rqr(y, pi=0.8, xi=0.2, m=8)
#' 
#' par(mfrow=c(1, 2))
#' car::qqPlot(r1, dist="norm", mean=0, sd=1, main='Correct model', las=1)
#' car::qqPlot(r2, dist="norm", mean=0, sd=1, main='Wrong model', las=1)
#' 
#' ks.test(r1, "pnorm", mean=0, sd=1)
#' ks.test(r2, "pnorm", mean=0, sd=1)
#' 
#' 
#' # Example 2, with covariates ----------------------------------------------
#' 
#' # Function to generate some random values from a CUB model
#' # pi explained by x1
#' # xi explained by x2
#' 
#' gendata <- function(n, m) {
#'   x1 <- runif(n)
#'   x2 <- rpois(n, lambda=4)
#'   pi <- pnorm(-1 + 2 * x1)
#'   xi <- pnorm( 4 - 1 * x2)
#'   y <- rcub(n=n, pi=pi, xi=xi, m=m)
#'   data.frame(y, x1, x2)
#' }
#' 
#' set.seed(12345)
#' dataset <- gendata(n=500, m=5)
#' 
#' # This is the correct model, pi explained by x1 and xi explained by x2
#' mod3 <- cub(pi.fo=y ~ x1, xi.fo= ~ x2, m=5, data=dataset)
#' 
#' # This is the wrong model
#' mod4 <- cub(pi.fo=y ~ x2, xi.fo=~ x1, m=5, data=dataset)
#' 
#' # The residuals for both models
#' r3 <- rqr(dataset$y, pi=mod3$fitted.pi, xi=mod3$fitted.xi, m=5)
#' r4 <- rqr(dataset$y, pi=mod4$fitted.pi, xi=mod4$fitted.xi, m=5)
#' 
#' par(mfrow=c(1, 2))
#' car::qqPlot(r3, dist="norm", mean=0, sd=1, main='Correct model', las=1)
#' car::qqPlot(r4, dist="norm", mean=0, sd=1, main='Wrong model', las=1)
#' 
#' ks.test(r3, "pnorm", mean=0, sd=1)
#' ks.test(r4, "pnorm", mean=0, sd=1)
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
