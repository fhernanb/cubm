
nrep <- 10
n <- 1000
mypi <- 0.15
myxi <- 0.60

res <- matrix(NA, nrow=nrep, ncol=6)

for (i in 1:nrep) {
  y <- rcub(n=n, pi=mypi, xi=myxi, m=5)
  mod1 <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m=5, shift=1,
              optimizer='nlminb')
  mod2 <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m=5, shift=1,
              optimizer='optim')
  mod3 <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m=5, shift=1,
              optimizer='DEoptim')
  res[i, ] <- c(mod1$par,
                mod2$par,
                mod3$optim$bestmem)
}


colMeans(pnorm(res))
apply(pnorm(res), 2, var)




