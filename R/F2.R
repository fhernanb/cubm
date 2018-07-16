#' F2 coefficient for cub model  
#' 
#' This function generates the F2 coefficient, difference 1 and Diss.
#'
#' @param mod object of cub class
#'
#' @examples 
#' #' # Example 1 
#' #' # Generating a random sample given the values of pi and xi
#' #' y <- rcub(n = 1000, pi = 0.15, xi = 0.60, m = 8)
#' #' mod <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m = 8, shift = 1)
#' #' F2(mod)
#' 
#' # Example 2 
#' fit <- cub(pi.fo = global ~ lage, xi.fo = ~ lage,  m = 7, data = univer)
#' F2(fit)
#'
#' # Example 3 
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
#' m <- 5
#' # Simulating the dataset
#' x1 <- runif(n)
#' x2 <- runif(n)
#' country <- sample(c('france', 'japan', 'usa'), size=n, replace=TRUE)
#' aux <- model.matrix(~country)[, -1] %*% matrix(c(beta_japan,  beta_usa))
#' pi <- pnorm(b0 + b1 * x1 + aux)
#' xi <- pnorm(g0 + g1 * x2 + aux)
#' y <- rcub(n=n, pi=pi, xi=xi, m=m)
#' # Dataset
#' dataset <- data.frame(y, x1, x2, country)
#' 
#' # Fitting the model
#' mod <- cub(pi.fo=y ~ x1 + country,
#'            xi.fo=~ x2 + country, m=5, data=dataset,
#'            optimizer='optim')
#' F2(mod)
#' 
#' @export
F2 <- function (mod){ 
  if (class(mod) != 'cub') 
    stop(paste("The object must be of class cub", "\n", ""))
  fr <- prop.table(table(mod$y))
  if (mod$p.pi == 1 && mod$p.xi == 1)
    p <- dcub(x = 1:mod$M, pi = mod$fitted.pi, xi = mod$fitted.xi, m = mod$M)
  else {
    aux.mat <- cbind(pi = mod$fitted.pi, xi = mod$fitted.xi, m = mod$M)
    myfun <- function(x) dcub(x=1:x[3], m=x[3], pi=x[1], xi=x[2])
    PRr <- t(apply(aux.mat, 1, myfun))
    p <- colSums(PRr) / mod$n
  }
  F2 <- 1-(0.5*sum(abs(fr - p)))
  return(F2)
}
