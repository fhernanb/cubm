#' bcub
#' 
#' Function to fit a cub model under a bayesian approach. The structure can have covariates or not to model \eqn{\pi} and \eqn{\xi} parameters.
#' 
#' @param pi.fo an object of class \code{\link[stats]{formula}}, a symbolic description of the model to fit pi parameter.
#' @param xi.fo an object of class \code{\link[stats]{formula}}, a symbolic description of the model to fit xi parameter without left side.
#' @param m the maximum value of the response variable.
#' @param shift the minimum value of the response variable.
#' @param data an optional data frame.
#' @param N This required argument accepts integers larger than 10, and determines the number of iterations that Laplace's Demon will update the parameters while searching for target distributions. 
#' @param pi.link link function to use for model pi parameter, by default is probit.
#' @param xi.link link function to use for model xi parameter, by default is probit.
#' 
#' @examples
#' 
#' 1 + 3
#' 
#' @export
#' 
bcub <- function(pi.fo, xi.fo, m, shift=1, data=NULL,
                 pi.link='probit', xi.link='probit', N=5000) {
  if(! pi.link %in% c('probit', 'logit')) 
    stop("That optimizer is wrong")
  if(! xi.link %in% c('probit', 'logit')) 
    stop("That optimizer is wrong")
  
  mf <- match.call(expand.dots = FALSE)
  matri <- model.matrix.cub(pi.fo, xi.fo, data)
  matri$m <- m
  beta.names <- paste('pi', colnames(matri$mat.pi), sep='_')
  gama.names <- paste('xi', colnames(matri$mat.xi), sep='_')
  parm.names <- c(beta.names, gama.names)
  matri$par.names <- parm.names
  matri$npar <- ncol(matri$mat.pi) + ncol(matri$mat.xi)

  data_list <- list(mon.names=c("postlik"),
                    parm.names=matri$par.names,
                    y=matri$y, m=matri$m,
                    mat.pi=matri$mat.pi,
                    mat.xi=matri$mat.xi,
                    pi.link=pi.link,
                    xi.link=xi.link)
  
  library(LaplacesDemon)
  res.LD <- LaplacesDemon(Model=model,
                          Data=data_list,
                          Iterations=N,
                          Algorithm="HARM",
                          Thinning=1,
                          Initial.Values=GIV(Model=model,
                                             Data=data_list,
                                             n=100,
                                             PGF=FALSE))
  results <- NULL
  results$res.LD <- res.LD
  results$data_list <- data_list
  results$matri <- matri
  class(results) <- "bcub"
  results
}

# The model specification
model <- function(parm, Data) {
  X.pi <- Data$mat.pi  # Model matrix to pi
  X.xi <- Data$mat.xi  # Model matrix to xi
  p.pi <- ncol(X.pi)   # Number of pi parameters
  p.xi <- ncol(X.xi)   # Number of xi parameters
  theta.pi <- matrix(parm[1:p.pi], ncol=1)    # Theta vector pi
  theta.xi <- matrix(parm[-(1:p.pi)], ncol=1) # Theta vector xi
  
  mu0 <- 0        # Hyperparameters for betas and gammas
  sigma0 <- 20  # Hyperparameters for betas and gammas
  
  pi <- ifelse(Data$pi.link=='probit',
               pnorm(X.pi %*% theta.pi),
               1 / (1 + exp(- X.pi %*% theta.pi)))
  
  xi <- ifelse(Data$xi.link=='probit',
               pnorm(X.xi %*% theta.xi),
               1 / (1 + exp(- X.xi %*% theta.xi)))
  
  loglik <- sum(dcub(Data$y, pi=pi, xi=xi, m=Data$m, log=TRUE))
  postlik <- loglik + sum(dnormv(parm, mu0, sigma0, log=TRUE))
  list(LP=postlik,
       Dev= -2 * loglik,
       Monitor=c(postlik), 
       yhat=NA, parm=parm)
}











