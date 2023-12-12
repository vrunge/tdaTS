

library(zipfR)
N1 <- 20
N2 <- 1
L1 <- 9
L2 <- 8

#

L1 <- 30
L2 <- 40
nb <- 12
ta <- runif(nb)<L1/(L1+L2)
N1 <- sum(ta == TRUE) + 1
N2 <- sum(ta == FALSE) + 1
c(N1,N2)

x <- seq(0,1,length.out = 100)
g <- N1*N2/(L1^N1*L2^N2)*(L1+L2)^(N1+N2)
g
l <- function(x) pmin(1,L2/(L1+L2-x*(L1+L2)))
r <- function(x) pmax(0,(L2-x*(L1+L2))/(L1+L2-x*(L1+L2)))

f_ibeta <- function(x) g*(Ibeta(l(x),N2,N1) - Ibeta(r(x),N2,N1))*(1-x)^(N1+N2-1)

x <- seq(0,1,length.out = 1000)
res <- NULL
for(i in 1:length(x))
{
  temp <- integrate(f_ibeta,0,x[i])
  res <- c(res, temp[[1]])
}
res
plot(x,res, type = 'l', col = 2, lwd = 4) ### verif that we converge toward 1

#######################################################


y <- x*(L1+L2)
x <- seq(0,1,length.out = 1000)
z <- seq(0,1,length.out = 1000)
#x[x>0.4] <- 0.4


r2 <- - (1-x)^(N1+N2+1)
r3 <- -(N1+N2+1) * x*(1-x)^(N1+N2)

res2 <- 1+r2+r3
plot(z,res2)


theMax <- max(res,res2)
plot(x,res, type = 'l', col = 1, lwd = 4, ylim = c(0,theMax))
par(new = TRUE)
plot(x,res2, type = 'l', col = 2, lwd = 4, ylim = c(0,theMax))


theMax <- max(res,res2)
plot(x,1-res, type = 'l', col = 1, lwd = 4, ylim = c(0,theMax)) #### the true result (using the beta function)
par(new = TRUE)
plot(x,1-res2, type = 'l', col = 3, lwd = 4, ylim = c(0,theMax)) #### the approximation in 0


max((abs(res2 - res)/res)[-1]) # max relative error
max(abs(res2 - res)) # max error (an error in probability values)
N1
N2
L1/N1 - L2/N2










#' plot_ECPC_1D_Unif
#'
#' @description Function plotting the Expectation curve ECPC with 1D Uniform data and its approximation
#' @param N1 Persistence diagram with 4 columns:  time, dimension, Birth, Death.
#' @param N2 Type of distance used
#' @param L1 Type of distance used
#' @param L2 Type of distance used
plot_ECPC_1D_Unif <- function(N1,N2,L1,L2)
{

  x <- seq(0,1,length.out = 100)
  g <- N1*N2/(L1^N1*L2^N2)*(L1+L2)^(N1+N2)

  l<- function(x) pmin(1,L2/(L1+L2-x*(L1+L2)))
  r <- function(x) pmax(0,(L2-x*(L1+L2))/(L1+L2-x*(L1+L2)))

  f_ibeta <- function(x) g*(Ibeta(l(x),N2,N1) - Ibeta(r(x),N2,N1))*(1-x)^(N1+N2-1)

  x <- seq(0,1,length.out = 1000)
  res <- NULL
  for(i in 1:length(x))
  {
    temp <- integrate(f_ibeta,0,x[i])
    res <- c(res, temp[[1]])
  }

  x <- seq(0,1,length.out = 1000)
  z <- seq(0,1,length.out = 1000)
  r2 <- - (1-x)^(N1+N2+1)
  r3 <- -(N1+N2+1) * x*(1-x)^(N1+N2)
  res2 <- 1+r2+r3


  theMax <- max(1-res,1-res2)
  plot(x,1-res, type = 'l', col = 1, lwd = 4, ylim = c(0,theMax)) #### the true result (using the beta function)
  par(new = TRUE)
  plot(x,1-res2, type = 'l', col = 3, lwd = 4, ylim = c(0,theMax)) #### the approximation in 0

  return(list(maxRelativeError  = max((abs(res2 - res)/res)[-1]),
              maxError = max(abs(res2 - res))))
}










