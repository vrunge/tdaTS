



dataUniform <- function(L1,L2,M,a,b,N)
{
  sel <- sample(c(1,0), size = N, replace = TRUE, prob = c(a,b))
  res <- runif(N,min = 0, max = L1)*sel + runif(N,min = L1 + M, max = L1 + M + L2)*(1-sel)
  return(res)
}

hist(dataUniform(L1 = 1, L2 =2, M = 1, a = 1, b = 1, N = 10000), breaks = 100)


data <- dataUniform(L1 = 1, L2 =2, M = 1, a = 1, b = 1, N = 10000)








euler <- function(r,N,N1,N2,L1,L2,M)
{
  res <- N
  res <- res - (N1-1)*(1-(1-pmin(2*r/L1,1))^N1)
  res <- res - (N2-1)*(1-(1-pmin(2*r/L2,1))^N2)

    y <- (2*r - M)/(L1+L2)
    r1 <- - (1-y)^(N1+N2+1)
    r2 <- -(N1+N2+1) * y*(1-y)^(N1+N2)
    res <- res - (1+r1+r2)*((2*r >= M))

  return(res)
}


library(zipfR)
euler2 <- function(r,N,N1,N2,L1,L2,M)
{
  res <- N
  res <- res - (N1-1)*(1-(1-pmin(2*r/L1,1))^N1)
  res <- res - (N2-1)*(1-(1-pmin(2*r/L2,1))^N2)

  g <- N1*N2/(L1^N1*L2^N2)*(L1+L2)^(N1+N2)
  f_l <- function(x) pmin(1,L2/(L1+L2-x*(L1+L2)))
  f_r <- function(x) pmax(0,(L2-x*(L1+L2))/(L1+L2-x*(L1+L2)))

  f_ibeta <- function(x) g*(Ibeta(f_l(x),N2,N1) - Ibeta(f_r(x),N2,N1))*(1-x)^(N1+N2-1)
  integ <- sapply(r, function(r) {u <- integrate(f_ibeta,0,min((2*r-M)/(L1+L2),1)); u[[1]]})
  res <- res - integ*((2*r >= M))

  return(res)
}






L1 <- 1
L2 <- 1
M <- 3

data <- dataUniform(L1 = L1, L2 = L2, M = M, a = 1, b = 1, N = 100)

plot(data)
res <- sort(diff(sort(data)))
r <- seq(0,1.2*max(res), length.out = 1000)  # for radius r
y <- rep(length(data), length(r))

for(i in 1:length(x)){y[i] <- y[i] - sum(res < 2*r[i])}
plot(r,(y), type = 'l', lwd = 2, col = 2)
segments(min(r[y == 2]), 2, max(r[y == 2]), 2, col= 3, lwd = 2)
par(new = TRUE)
plot(r,euler(r,length(data),sum(data <=L1) ,sum(data > L1),L1,L2,M), type = 'l')





######


L1 <- 2
L2 <- 1
M <- 1
N <- 100
a <- 2
b <- 2
maxX <- M + min(c(L1,L2))

y <- rep(0, 1000)
w <- rep(0, 1000)

nb <- 1000

for(i in 1:nb)
{
  data <- dataUniform(L1 = L1, L2 = L2, M = M, a = a, b = b, N = N)
  res <- sort(diff(sort(data)))
  r <- seq(0,maxX, length.out = 1000)
  yNEW <- rep(N, 1000)

  for(i in 1:1000){yNEW[i] <- yNEW[i] - sum(res <= 2*r[i])}
  y <- y + yNEW
  w <- w + yNEW^2
}

z <- y/nb

plot((r),log(z), type = 'l', lwd = 2, col = 1, ylim = log(c(1,length(data))))
par(new = TRUE)
plot((r),log(euler2(r,N,N*a/(a+b) ,N*b/(a+b) ,L1,L2,M)), type = 'l', ylim = log(c(1,length(data))), col = 2, lwd = 2)
par(new = TRUE)
plot(r, log(euler(r,N,N*a/(a+b) ,N*b/(a+b) ,L1,L2,M)), type = 'l', ylim = log(c(1,length(data))), col = 3, lwd = 2)

plot((r),(z), type = 'l', lwd = 2, col = 1, ylim = (c(1,length(data))))
par(new = TRUE)
plot((r),(euler2(r,N,N*a/(a+b) ,N*b/(a+b) ,L1,L2,M)), type = 'l', ylim = (c(1,length(data))), col = 2, lwd = 2)


####################################################################################################
####### variance
####################

eulerVariance <- function(r,N,L)
{
  A <- (N-1)*(1-pmin(2*r/L,1))^N*(1 -(1-pmin(2*r/L,1))^N)
  B <- -(N-1)*(N-2)*(1 -(1-pmin(2*r/L,1))^N)^2
  C <- (N-1)*(N-2)*(1 - 2*(1-pmin(2*r/L,1))^N +(1 - pmin(2*r/L,1) - pmin(2*r/L,1-(2*r/L)))^N)
  return(A+B+C)
}

eulerVariance2 <- function(r,N,L)
{
  A <- (N-1)*(1-pmin(2*r/L,1))^N*(1 -(N-1)*(1-pmin(2*r/L,1))^N)
  C <- (N-1)*(N-2)*(1 - pmin(4*r/L,1))^N
  return(A+C)
}

eulerVariance3 <- function(r,N,L)
{
  A <- (1-pmin(2*r/L,1))^N*(1 -(1-pmin(2*r/L,1))^N)
  C <- (N-2)*((1-pmin(4*r/L,1))^N - (1-pmin(2*r/L,1))^(2*N))
  return((N-1)*(A+C))
}


L <- 1
N <- 200
y <- rep(0, 1000)
w <- rep(0, 1000)

nb <- 1000
r <- seq(0,L/2, length.out = 1000)
for(i in 1:nb)
{
  data <- runif(N,0,L)
  res <- sort(diff(sort(data)))
  yNEW <- rep(N, 1000)
  for(i in 1:1000){yNEW[i] <- yNEW[i] - sum(res <= 2*r[i])}
  y <- y + yNEW
  w <- w + yNEW^2
}

myVar <- w/nb - (y/nb)^2
plot(r,  y/nb, type = 'l', lwd = 2)

### sqrt(VAR)/ N

yylim <-  c(0,max(sqrt(myVar),sqrt(abs(eulerVariance(r,N,L)))))
plot(r,  sqrt(myVar), type = 'l', lwd = 2, col = 1, ylim =yylim)
par(new = TRUE)
plot(r, sqrt(abs(eulerVariance(r,N,L))), type = 'l', lwd = 2, col = 3, ylim = yylim)
par(new = TRUE)
plot(r, sqrt(abs(eulerVariance2(r,N,L))), type = 'l', lwd = 2, col = 2, ylim = yylim)
par(new = TRUE)
plot(r, sqrt(abs(eulerVariance3(r,N,L))), type = 'l', lwd = 2, col = 4, ylim = yylim)

yylim <-  c(0,max((myVar),(abs(eulerVariance(r,N,L)))))
plot(r,  (myVar), type = 'l', lwd = 2, col = 1, ylim =yylim)
par(new = TRUE)
plot(r, ((eulerVariance(r,N,L))), type = 'l', lwd = 2, col = 3, ylim = yylim)
par(new = TRUE)
plot(r, ((eulerVariance2(r,N,L))), type = 'l', lwd = 2, col = 2, ylim = yylim)
par(new = TRUE)
plot(r, ((eulerVariance3(r,N,L))), type = 'l', lwd = 2, col = 4, ylim = yylim)


max(myVar)
max(eulerVariance3(r,N,L))
3*N/20
N/10

s <- 1/((10/L)*(N-1))
u <- (1-pmin(2*s/L,1))^N*(1 -(1-pmin(2*s/L,1))^N)
v <- (N-2)*((1-pmin(4*s/L,1))^N - (1-pmin(2*s/L,1))^(2*N))
(N-1)*(u+v)
exp(-1/5)*(1-exp(-1/5))*(N-1)


exp(-1/5 * (N)/(N-1))*(1-exp(-1/5* (N)/(N-1)))*(N-1)
max(eulerVariance3(r,N,L))
