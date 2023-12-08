



dataUniform <- function(L1,L2,M,a,b,N)
{
  sel <- sample(c(1,0), size = N, replace = TRUE, prob = c(a,b))
  res <- runif(N,min = 0, max = L1)*sel + runif(N,min = L1 + M, max = L1 + M + L2)*(1-sel)
  return(res)
}

hist(dataUniform(L1 = 1, L2 =2, M = 1, a = 1, b = 1, N = 10000), breaks = 100)


data <- dataUniform(L1 = 1, L2 =2, M = 1, a = 1, b = 1, N = 10000)


EulerCurve_Uniform_1D <- function(data)
{

  return(res)
}







euler <- function(r,N,N1,N2,L1,L2,M)
{
  res <- N
  res <- res - (N1-1)*(1-(1-pmin(2*r/L1,1))^N1)
  res <- res - (N2-1)*(1-(1-pmin(2*r/L2,1))^N2)

    y <- (2*r - M)/(L1+L2)
    r2 <- - (1-y)^(N1+N2+1)
    r3 <- -(N1+N2+1) * y*(1-y)^(N1+N2)
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
L2 <- 2
M <- 1

data <- dataUniform(L1 = L1, L2 = L2, M = M, a = 2, b = 1, N = 100)


res <- sort(diff(sort(data)))
r <- seq(0,1.2*max(res), length.out = 1000)  # for radius r
y <- rep(length(data), length(x))

for(i in 1:length(x)){y[i] <- y[i] - sum(res < 2*r[i])}
plot(r,y, type = 'l', lwd = 2, col = 2)
segments(min(r[y == 2]), 2, max(r[y == 2]), 2, col= 3, lwd = 2)
par(new = TRUE)
plot(r,euler(r,length(data),sum(data <=L1) ,sum(data > L1),L1,L2,M), type = 'l')





######


L1 <- 2
L2 <- 4
M <- 0.5
N <- 100
a <- 4
b <- 2
maxX <- M + min(c(L1,L2))

y <- rep(0, 1000)

nb <- 1000

for(i in 1:nb)
{
  data <- dataUniform(L1 = L1, L2 = L2, M = M, a = a, b = b, N = N)
  res <- sort(diff(sort(data)))
  r <- seq(0,maxX, length.out = 1000)
  yNEW <- rep(N, 1000)

  for(i in 1:1000){yNEW[i] <- yNEW[i] - sum(res <= 2*r[i])}
  y <- y + yNEW
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


