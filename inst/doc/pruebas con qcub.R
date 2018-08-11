# con poisson
x <- seq(-2, 7, 0.01)
plot(x, ppois(x, 2), type="s", ylab="F(x)", 
     main="Poisson(2) CDF", las=1)
grid()

qpois(p=c(0.12, 0.22, 0.55, 0.78), lambda=2)

abline(h=0.12, col='red')
abline(h=0.22, col='blue')
abline(h=0.55, col='green')
abline(h=0.78, col='purple')

# con cub
pi <- 0.7
xi <- 0.1
m <- 10

x <- seq(-2, m+2, 0.01)
plot(x, pcub(x, pi=pi, xi=xi, m=m), type="s", ylab="F(x)", 
     main="CUB", las=1)
grid()

miscuantiles <- runif(n=3)

for(i in 1:length(miscuantiles)) abline(h=miscuantiles[i], col=i)

aux <- function(p, pi, xi, m) {
  cdf <- pcub(q=0:(m+1), pi=pi, xi=xi, m=m)
  findInterval(p, cdf)
}

aux <- Vectorize(aux)

aux(p=miscuantiles, pi, xi, m)
qcub(p=miscuantiles, pi=pi, xi=xi, m=m) # ahora si esta correcto

cbind(miscuantiles, aux(p=miscuantiles, pi, xi, m))
