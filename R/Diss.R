#' The dissimilarity index Diss for cub model
#' 
#' This function generates Diss coefficient, difference between observed relative frequencies and estimated (theoretical) probabilities.
#'
#' @param mod object of cub class
#'
#' @examples 
#'
#' # Test 1 
#' 
#'y <- rcub(n = 800, pi = 0.30, xi = 0.80, m = 6)
#'mod <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m = 6, shift = 1, optimizer = "optim")
#'Diss(mod)
#' 
#' # Test 2
#' 
#'fit <- cub(pi.fo = global ~ gender + lage, xi.fo = ~ willingn + residenc, m = 7, data = univer)
#'Diss(fit)
#'
# Diss function ------------------------------------------------------------
#' @export
Diss <- function (mod)
{ 
  fr <- table(mod$y)/length(mod$y) 
  fr <- as.vector(fr)      
  paux <- function(x){
    dcub(x = 1:x[3], m = x[3], pi = x[1], xi = x[2])}
  if (mod$p.pi != 1 && mod$p.xi != 1){
    para <- cbind(mod$fitted.pi, mod$fitted.xi, mod$M)
    para1 <- apply(para, 1, paux)
    para2 <- apply(para1, 2, which.max)  
    p <- prop.table(table(para2))
    p <- as.vector(p)}
  else if (mod$p.pi == 1 && mod$p.xi != 1 && mod$pi.link == 'probit')
  {pi = pnorm(mod$par)[1]
  para <- cbind(pi, mod$fitted.xi, mod$M)
  para1 <- apply(para, 1, paux)
  para2 <- apply(para1, 2, which.max)  
  p <- prop.table(table(para2))
  p <- as.vector(p)}
  else if (mod$p.pi != 1 && mod$p.xi == 1 && mod$pi.link == 'probit')
  {xi = pnorm(mod$par)[2]
  para <- cbind(mod$fitted.pi, xi, mod$M)
  para1 <- apply(para, 1, paux)
  para2 <- apply(para1, 2, which.max)  
  p <- prop.table(table(para2))
  p <- as.vector(p)}
  else if (mod$p.pi == 1 && mod$p.xi != 1 && mod$pi.link == 'logit')
  {pi = 1/(1 + exp(-mod$par[1]))
  para <- cbind(pi, mod$fitted.xi, mod$M)
  para1 <- apply(para, 1, paux)
  para2 <- apply(para1, 2, which.max)  
  p <- prop.table(table(para2))
  p <- as.vector(p)}
  else if (mod$p.pi != 1 && mod$p.xi == 1 && mod$pi.link == 'logit')
  {xi = 1/(1 + exp(-mod$par[2]))
  para <- cbind(mod$fitted.pi, xi, mod$M)
  para1 <- apply(para, 1, paux)
  para2 <- apply(para1, 2, which.max)  
  p <- prop.table(table(para2))
  p <- as.vector(p)}
  else if (mod$p.pi == 1 && mod$p.xi == 1 && mod$pi.link == 'probit')
    p <- dcub(pi = pnorm(mod$par)[1], xi = pnorm(mod$par)[2], 
             x = 1:mod$M, m = mod$M) 
  else 
    p <- dcub(pi = 1/(1 + exp(-mod$par[1])), xi = 1/(1 + exp(-mod$par[2])), 
             x = 1:mod$M, m = mod$M) 
  Diss <- (0.5 * sum(abs(fr - p)))
  return(Diss)
  }





