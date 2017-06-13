# In this file we compare the results from optim, DEoptim
# and nlminb to fit the model with covariates for pi

require(cubm)
rcub.covariates <- function(n, b0, b1, xi, m = 5, shift = 1) {
  x1 <- runif(n)
  pi <- pnorm(b0 + b1 * x1)
  xi <- xi
  y <- rcub(n = n, pi = pi, xi = xi, m = m, shift = shift)
  data.frame(y, x1)
}

# loglikelihood function
llcub <- function(theta, y, M, X.pi, X.xi) {
  b0 <- theta[1]
  b1 <- theta[2]
  betas.pi <- matrix(theta[1:ncol(X.pi)], ncol=1)
  betas.xi <- matrix(theta[-(1:ncol(X.pi))], ncol=1)
  pi <- pnorm(X.pi %*% betas.pi)
  xi <- pnorm(X.xi %*% betas.xi)
  ll <- sum(dcub(pi=pi, xi=xi, x=y, m=M, log=TRUE))
  -ll  # minus to use with optim/nlminb function
}

# Generating the data
n <- 1000
dataset <- rcub.covariates(n=n, b0=-2, b1=2, xi=0.35)
theta <- c(-2, 2, qnorm(0.35)) 
llcub(theta, y=dataset$y, M=5,
      X.pi=cbind(1, dataset$x1), X.xi=matrix(1, nrow=n))

# optim
res1 <- optim(par=c(0, 0, 0), fn=llcub,
              y=dataset$y, M=5,
              X.pi=cbind(1, dataset$x1), X.xi=matrix(1, nrow=n))

c(res1$par[1:2], pnorm(res1$par[3]))

# DEoptim
require(DEoptim)
DEcontrol=list(trace=0, itermax=200)
res2 <- DEoptim(fn=llcub, lower=rep(-10, 3), upper=rep(10, 3),
                control=DEcontrol,
                dataset$y, 5,
                cbind(1, dataset$x1), matrix(1, nrow=n))
c(res2$optim$bestmem[1:2], pnorm(res2$optim$bestmem[3]))

# nlminb
res3 <- nlminb(start=c(0, 0, 0), objective=llcub,
               y=dataset$y, M=5,
               X.pi=cbind(1, dataset$x1), X.xi=matrix(1, nrow=n))
c(res3$par[1:2], pnorm(res3$par[3]))

# Simulation study --------------------------------------------------------

nrep <- 100
results <- matrix(NA, ncol=6, nrow=nrep)

for (i in 1:nrep) {
  n <- 1000
  dataset <- rcub.covariates(n=n, b0=-2, b1=2, xi=0.35)
  res1 <- optim(par=c(0, 0, 0), fn=llcub,
                y=dataset$y, M=5,
                X.pi=cbind(1, dataset$x1), X.xi=matrix(1, nrow=n))
  res2 <- DEoptim(fn=llcub, lower=rep(-10, 3), upper=rep(10, 3),
                  control=DEcontrol,
                  dataset$y, 5,
                  cbind(1, dataset$x1), matrix(1, nrow=n))
  r1 <- c(res1$par[1:2], pnorm(res1$par[3]))
  r2 <- c(res2$optim$bestmem[1:2], pnorm(res2$optim$bestmem[3]))
  results[i, ] <- c(r1, r2)
  print(i)
}

round(results, 3)
colMeans(results)









