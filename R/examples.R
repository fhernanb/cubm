
#----------------------------------
y <- rcub(n=1000, pi=0.15, xi=0.60, m=5)
mod1 <- bcub(pi.fo = y ~ 1, xi.fo = ~ 1, m=5, shift=1)
mod1
pnorm(mod1[1:2, 1])

mod3 <- cub(pi.fo = y ~ 1, xi.fo = ~ 1, m=5, shift=1,
            optimizer='nlminb')
summary(mod3)
pnorm(mod3$par)

#----------------------------------
rcub.covariates <- function(n, b0, b1, g0, g1, m = 5, shift = 1) {
  x1 <- runif(n)
  x2 <- runif(n)
  pi <- pnorm(b0 + b1 * x1)
  xi <- pnorm(g0 + g1 * x2)
  y <- rcub(n = n, pi = pi, xi = xi, m = m, shift = shift)
  data.frame(y, x1, x2)
}

# Generating the data
dataset <- rcub.covariates(n=1000, b0=-1, b1=1, g0=-2, g1=1.5)
# Fitting the model
mod2 <- bcub(pi.fo = y ~ x1, xi.fo = ~ x2, m=5, data=dataset)
mod2

mod4 <- cub(pi.fo = y ~ x1, xi.fo = ~ x2, m=5, data=dataset)
mod4
#----------------------------------

