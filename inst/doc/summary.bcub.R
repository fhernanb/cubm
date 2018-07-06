#' summary.bcub
#' 
#' Summarizes a cub model.
#' 
#' @param mod An object of class \code{bcub}.
#' 
#' @examples 
#' # rcub.covariates is a function to generate a random sample from a cub model 
#' # using covariates to model pi and xi and link function qnorm
#' rcub.covariates <- function(n, b0, b1, g0, g1, m = 5, shift = 1) {
#'   x1 <- runif(n)
#'   x2 <- runif(n)
#'   pi <- pnorm(b0 + b1 * x1)
#'   xi <- pnorm(g0 + g1 * x2)
#'   y <- rcub(n = n, pi = pi, xi = xi, m = m, shift = shift)
#'   data.frame(y, x1, x2)
#' }
#' 
#' # Generating the data
#' dataset <- rcub.covariates(n=1000, b0=-1, b1=1, g0=-2, g1=1.5)
#' # Fitting the model
#' mod <- cub(pi.fo = y ~ x1, xi.fo = ~ x2, m=5, data=dataset, optimizer='nlminb')
#' summary(mod)
#' 
#' @export
#' 
# summary function --------------------------------------------------------
summary.bcub <- function(mod) {
  .myenv <- environment()
  var.list <- as.list(mod)
  list2env(var.list , envir = .myenv)
  
  p.pi <- ncol(mod$matri$mat.pi) # Number of parameters for pi
  estimate <- mod$res.LD$Summary1[1:mod$matri$npar, 1]
  se       <- mod$res.LD$Summary1[1:mod$matri$npar, 2]
  pi.link <- mod$data_list$pi.link
  xi.link <- mod$data_list$xi.link
  
  zvalue   <- estimate / se
  pvalue   <- 2 * pnorm(abs(zvalue), lower.tail=F)
  res      <- cbind(estimate=estimate, se=se, zvalue=zvalue, pvalue=pvalue)
  colnames(res) <- c('Estimate', 'Std. Error', 't value', 'Pr(>|t|)')
  res      <- as.data.frame(res)
  cat("---------------------------------------------------------------\n")
  cat(paste("-------------- Fixed effects for ",
            pi.link, "(pi) --------------------\n", sep=''))
  cat("---------------------------------------------------------------\n")
  printCoefmat(res[1:p.pi,], P.value=TRUE, has.Pvalue=TRUE)
  cat("---------------------------------------------------------------\n")
  cat(paste("-------------- Fixed effects for ",
            xi.link, "(xi) --------------------\n", sep=''))
  cat("---------------------------------------------------------------\n")
  printCoefmat(res[-(1:p.pi),], P.value=TRUE, has.Pvalue=TRUE)
  cat("---------------------------------------------------------------\n")
  cat("---------------------------------------------------------------\n")
}

