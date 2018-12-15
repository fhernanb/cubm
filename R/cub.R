#' cub
#' 
#' function to fit a cub model. The user could use covariates or not to model the parameters \eqn{\pi} and \eqn{\xi}.
#' 
#' @aliases summary.cub
#' 
#' @param pi.fo an object of class \code{\link[stats]{formula}}, a symbolic description of the model to fit pi parameter.
#' @param xi.fo an object of class \code{\link[stats]{formula}}, a symbolic description of the model to fit xi parameter without left side.
#' @param m the maximum value of the response variable.
#' @param data an optional data frame.
#' @param subset an optional vector specifying a subset of observations to be used. For example, \code{subset = "sex =='male'"} means that we want to use only the male data set.
#' @param optimizer two options are available: \code{\link[stats]{nlminb}} (default), \code{\link[stats]{optim}} or \code{\link[DEoptim]{DEoptim}}.
#' @param pi.link link function to use for model pi parameter, two options are available, logit or probit, by default is probit.
#' @param xi.link link function to use for model xi parameter, two options are available, logit or probit, by default is probit.
#' @param initial.values vector with the initial values to the searching procedure to nlminb and optim optimizers.
#' @param ... Further arguments to be passed to \code{\link[DEoptim]{DEoptim.control}}.
#' 
#' @examples
#' x <- 1:5
#' y <- rnorm(5)
#' mod <- lm(y ~ x)
#' 
#' @importFrom stats model.matrix model.frame nlminb optim pnorm as.formula sd printCoefmat
#' @importFrom DEoptim DEoptim
#' @export
#' 
# cub function ------------------------------------------------------------
cub <- function(pi.fo, xi.fo, m, data=NULL, subset=NULL,
                optimizer='nlminb',
                pi.link='probit', xi.link='probit', initial.values=NULL, ...) {
  if(! optimizer %in% c('nlminb', 'optim', 'DEoptim')) 
    stop("That optimizer is wrong")
  if(! pi.link %in% c('probit', 'logit')) 
    stop("That optimizer is wrong")
  if(! xi.link %in% c('probit', 'logit')) 
    stop("That optimizer is wrong")
  if (!is.null(subset)) data <- subset(data, eval(parse(text=subset)))
  
  matri <- model.matrix.cub(pi.fo, xi.fo, data)
  res <- fit.cub(matri, m=m, optimizer=optimizer, 
                 pi.link=pi.link, xi.link=xi.link,
                 initial.values, ...)
  res$pi.fo <- pi.fo
  res$xi.fo <- xi.fo
  res$parameters <- c('pi', 'xi')
  res$call <- match.call(expand.dots = FALSE)
  
  # To recuparate the data.frame
  mf1 <- model.frame(formula=pi.fo, data=data)
  if (xi.fo[2] != '1()') {
    mf2 <- model.frame(formula=xi.fo, data=data)
    res$model <- cbind(mf1, mf2)
  }
  else res$model <- mf1
  
  class(res) <- "cub"
  res
}
#'
# model.matrix.cub --------------------------------------------------------
model.matrix.cub <- function(pi.fo, xi.fo, data=NULL) {
  stopifnot (class(pi.fo) == 'formula')
  stopifnot (class(xi.fo) == 'formula')
  response <- all.vars(pi.fo)[1]
  xi.fo <- as.formula(paste(response, paste(as.character(xi.fo),
                                            collapse='')))
  mat.pi <- model.matrix(pi.fo, data)
  mat.xi <- model.matrix(xi.fo, data)
  y <- model.frame(pi.fo, data=data)[, 1]
  list(mat.pi=mat.pi, mat.xi=mat.xi, y=y)
}
#'
# fit.cub -----------------------------------------------------------------
fit.cub <- function(matri, m, optimizer, pi.link, xi.link,
                    initial.values, ...) {
    p.pi <- ncol(matri$mat.pi)  # Number of pi parameters
    p.xi <- ncol(matri$mat.xi)  # Number of xi parameters
    X.pi <- matri$mat.pi  # Model matrix to pi
    X.xi <- matri$mat.xi  # Model matrix to xi
    y <- matri$y  # Response variable
    names.pi <- colnames(matri$mat.pi)
    names.xi <- colnames(matri$mat.xi)
    if (is.null(initial.values) | length(initial.values) != (p.pi+p.xi))
      initial.values <- rep(0, p.pi+p.xi)
    
    if (optimizer == 'nlminb') {
      nlminbcontrol <- list(...)
      fit <- nlminb(start=initial.values, objective=llcub,
                    y=y, M=m, X.pi=X.pi, X.xi=X.xi,
                    pi.link=pi.link, xi.link=xi.link,
                    control=nlminbcontrol)
      fit$objective <- -fit$objective
    }
    
    if (optimizer == 'optim') {
      optimcontrol <- list(...)
      fit <- optim(par=initial.values, fn=llcub,
                   y=y, M=m, X.pi=X.pi, X.xi=X.xi,
                   pi.link=pi.link, xi.link=xi.link,
                   control=optimcontrol)
      fit$objective <- -fit$value
    }
    
    if (optimizer == 'DEoptim') {
      DEcontrol <- list(...)
      fit <- DEoptim(fn=llcub,
                     lower=rep(-10, p.pi+p.xi),
                     upper=rep( 10, p.pi+p.xi),
                     control=DEcontrol,
                     y, m, X.pi, X.xi,
                     pi.link, xi.link)
      fit$par <- fit$optim$bestmem
      fit$objective <- -fit$optim$bestval
    }
    
  # Unifying the results
  names(fit$par) <- c(names.pi, names.xi)
  # Obtaining fitted pi and xi
  fit$fitted.pi <- fitted.pi(p.pi, p.xi, pi.link, X.pi, X.xi, fit)
  fit$fitted.xi <- fitted.xi(p.pi, p.xi, xi.link, X.pi, X.xi, fit)
  # Obtaining the hessian
  fit$Hessian <- numDeriv::hessian(func=llcub, x=fit$par,
                                   method='Richardson',
                                   y=y, M=m,
                                   X.pi=X.pi, X.xi=X.xi)
  inputs <- list(y=y, M=m, log=TRUE, p.pi=p.pi, p.xi=p.xi,
                 n=length(y), 
                 X.pi=X.pi,
                 X.xi=X.xi,
                 pi.link=pi.link,
                 xi.link=xi.link)
  fit <- c(fit, inputs)
}
#'
# llcub -------------------------------------------------------------------
# This function calculates the minus log-likelihood
llcub <- function(theta, y, M, X.pi, X.xi,
                  pi.link='probit', xi.link='probit') {
  betas.pi <- matrix(theta[1:ncol(X.pi)], ncol=1)
  #betas.xi <- matrix(theta[-(1:ncol(X.pi))], ncol=1)
  betas.xi <- matrix(theta[(ncol(X.pi)+1) : (ncol(X.pi)+ncol(X.xi))], ncol=1)
  pi <- pnorm(X.pi %*% betas.pi)
  xi <- pnorm(X.xi %*% betas.xi)
  if (pi.link == 'logit') pi <- 1 / (1 + exp(- X.pi %*% betas.pi))
  if (xi.link == 'logit') xi <- 1 / (1 + exp(- X.xi %*% betas.xi))
  ll <- sum(dcub(pi=pi, xi=xi, x=y, m=M, log=TRUE))
  -ll  # minus to use with optim/nlminb function
}
#'
# fitted.pi and fitted.xi -------------------------------------------------
fitted.pi <- function(p.pi, p.xi, pi.link, X.pi, X.xi, fit) {
  betas.pi <- matrix(fit$par[1:ncol(X.pi)], ncol=1)
  betas.xi <- matrix(fit$par[-(1:ncol(X.pi))], ncol=1)
  if  (p.pi != 1 && pi.link =='probit')
  {pi <- pnorm(X.pi %*% betas.pi)}
  else if (p.pi == 1 && pi.link == 'probit')
  {pi <- pnorm(betas.pi)}
  else if (p.pi != 1 && pi.link =='logit')
  {pi <- 1/(1 + exp(-(X.pi %*% betas.pi)))}
  else pi <- 1/(1 + exp(-betas.pi))  
  return(as.numeric(pi))    
}
#'
fitted.xi <- function(p.pi, p.xi, xi.link, X.pi, X.xi, fit) {
  betas.pi <- matrix(fit$par[1:ncol(X.pi)], ncol=1)
  betas.xi <- matrix(fit$par[-(1:ncol(X.pi))], ncol=1)
  if (p.xi != 1 && xi.link =='probit')
  {xi <- pnorm(X.xi %*% betas.xi)} 
  else if (p.xi == 1 && xi.link == 'probit')
  {xi <- pnorm(betas.xi)}
  else if (p.xi != 1 && xi.link =='logit')
  {xi <- 1/(1 + exp(-(X.pi %*% betas.xi)))}
  else xi <- 1/(1 + exp(-betas.xi))
  return(as.numeric(xi))
}
# -----------------------------------------------------------------
# --------------------- summary function --------------------------
# -----------------------------------------------------------------
#' @importFrom stats pnorm update
#' @importFrom boot boot
#' @export
#' 
summary.cub <- function(object, ...) {
  .myenv <- environment()
  var.list <- as.list(object)
  list2env(var.list , envir = .myenv)
  estimate <- object$par
  elements <- sqrt(diag(solve(object$Hessian))) # diagonal of Hessian^-1
  if (any(is.na(elements))) se <- boot_cub(object=object)
  else se <- elements
  zvalue   <- estimate / se
  pvalue   <- 2 * pnorm(abs(zvalue), lower.tail=F)
  res      <- cbind(estimate=estimate, se=se, zvalue=zvalue, pvalue=pvalue)
  res      <- data.frame(res)
  colnames(res) <- c('Estimate', 'Std. Error', 'z value', 'Pr(>|z|)')
  res.pi <- res[1:p.pi,]
  res.xi <- res[-(1:p.pi),]
  rownames(res.pi) <- names(object$par)[1:p.pi]
  rownames(res.xi) <- names(object$par)[-(1:p.pi)]
  p.pi <- object$p.pi
  cat("---------------------------------------------------------------\n")
  cat(paste("Fixed effects for ",
            object$pi.link, "(pi) \n", sep=''))
  cat("---------------------------------------------------------------\n")
  printCoefmat(res.pi, P.values=TRUE, has.Pvalue=TRUE)
  cat("---------------------------------------------------------------\n")
  cat(paste("Fixed effects for ",
            object$xi.link, "(xi) \n", sep=''))
  cat("---------------------------------------------------------------\n")
  printCoefmat(res.xi, P.values=TRUE, has.Pvalue=TRUE)
  cat("---------------------------------------------------------------\n")
}
# Bootstrap
# This function is used to obtain standard error for betas
# by bootstrap method.
boot_cub <- function(object) {
  datos <- object$model
  fun <- function(data, indices, modelo) {
    dt <- data[indices, , drop=FALSE]
    aux <- update(modelo, data=dt)
    aux$par
  }
  res <- boot::boot(data=datos, statistic=fun, R=1000, modelo=object)
  return(apply(res$t, 2, sd))
}
# -----------------------------------------------------------------
# ---------------------  print function ---------------------------
# -----------------------------------------------------------------
#' @export
print.cub <- function(x, ...) {
  cat("Call:\n")
  print(x$call)
  cat("\n Results: \n")
  cat("\n Estimated coefficients for g(pi): \n")
  print(x$par[1:x$p.pi])
  cat("\n Estimated coefficients for g(xi): \n")
  print(x$par[-(1:x$p.pi)])
}


