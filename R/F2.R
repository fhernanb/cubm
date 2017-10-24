#' F2 coefficient for cub model  
#' 
#' This function generates the F2 coefficient, difference 1 and Diss.
#'
#' @param mod object of cub class
#'
#' @examples 
#' 
#' # Test 1 
#' 
#' # Generating a random sample given the values of pi and xi
#' 
#' y <- rcub(n = 1000, pi = 0.15, xi = 0.60, m = 8)
#' mod <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m = 8, shift = 1)
#' F2.cub(mod)
#' 
#' # Test 2 
#' 
#' fit <- cub(pi.fo = usecondom ~ gender, xi.fo = ~ gender,  m = 4, data = data)
#' F2.cub(fit)
#'
#' # Test 3 
#' 
#' # Simulating a dataset with qualitative variables
#' n <- 10000
#' # Betas for pi
#' b0 <- -1
#' b1 <- 1
#' # Betas for xi
#' g0 <- -2
#' g1 <- 1.5
#' 
#' beta_japan <- 1 # Coefficient for japan
#' beta_usa <- -0.7 # Coefficient for usa, france is reference level
#' 
#' # m and shift value
#' m <- 5
#' shift <- 1
#' 
#' # Simulating the dataset
#' x1 <- runif(n)
#' x2 <- runif(n)
#' country <- sample(c('france', 'japan', 'usa'), size = n, replace = T)
#' aux <- model.matrix(~country)[, -1] %*% matrix(c(beta_japan,  beta_usa))
#' pi <- pnorm(b0 + b1 * x1 + aux)
#' xi <- pnorm(g0 + g1 * x2 + aux)
#' y <- rcub(n = n, pi = pi, xi = xi, m = m, shift = shift)
#' # Dataset
#' dataset <- data.frame(y, x1, x2, country)
#' 
#' # Fitting the model
#' mod <- cub(pi.fo = y ~ x1 + country,
#'            xi.fo = ~ x2 + country, m = 5, data = dataset,
#'            optimizer = 'optim')
#' F2.cub(mod)
#'
# F2.cub function ------------------------------------------------------------
#' @export
F2.cub<-function (mod)
{ 
      ccalc <- function(para){
      para1 <- apply(para, 1, paux)
      para2 <- apply(para1, 2, which.max)
      para3 <- table(para2)
      para4 <- sapply(1:length(fr), function(x) sum(para2[para2 == x]))
      para4[which(para4 != 0)] <- as.vector(prop.table(para3))
      return(para4)
    }
    
    fr <- table(mod$y)/length(mod$y) 
    fr <- as.vector(fr)      
    paux <- function(x){
      dcub(x = 1:x[3], m = x[3], pi = x[1], xi = x[2])}
    if (mod$p.pi != 1 && mod$p.xi != 1){
      para <- cbind(mod$fitted.pi, mod$fitted.xi, mod$M)
      p <- ccalc(para)}
    else if (mod$p.pi == 1 && mod$p.xi != 1 && mod$pi.link == 'probit')
    {pi = pnorm(mod$par)[1]
    para <- cbind(pi, mod$fitted.xi, mod$M)
    p <- ccalc(para)}
    else if (mod$p.pi != 1 && mod$p.xi == 1 && mod$pi.link == 'probit')
    {xi = pnorm(mod$par)[2]
    para <- cbind(mod$fitted.pi, xi, mod$M)
    p <- ccalc(para)}
    else if (mod$p.pi == 1 && mod$p.xi != 1 && mod$pi.link == 'logit')
    {pi = 1/(1 + exp(-mod$par[1]))
    para <- cbind(pi, mod$fitted.xi, mod$M)
    p <- ccalc(para)}
    else if (mod$p.pi != 1 && mod$p.xi == 1 && mod$pi.link == 'logit')
    {xi = 1/(1 + exp(-mod$par[2]))
    para <- cbind(mod$fitted.pi, xi, mod$M)
    p <- ccalc(para)}
    else if (mod$p.pi == 1 && mod$p.xi == 1 && mod$pi.link == 'probit')
      p <- dcub(pi = pnorm(mod$par)[1], xi = pnorm(mod$par)[2], 
                x = 1:mod$M, m = mod$M) 
    else 
      p <- dcub(pi = 1/(1 + exp(-mod$par[1])), xi = 1/(1 + exp(-mod$par[2])), 
                x = 1:mod$M, m = mod$M) 
    F2 <- 1-(0.5 * sum(abs(fr - p)))
    return(F2)
  }
  
          
  


