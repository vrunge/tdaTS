

############################################################
### radius density (boundary density)
############################################################

th <- 2*pi*0.1
N <- 1000
alpha <- 0.1
Bound <- 3
r <- seq(0, Bound, length.out = 100)
res <- r^3*(2*pi-th)^3*(1-r^2*(2*pi-th-sin(th))/(2*alpha*N))^(N-3)*r*sin(th/2)
plot(r,res, type = 'b')
res
par(new = TRUE)
res2 <- r^3*(2*pi-th)^3*exp(-r^2*(2*pi-th-sin(th))/(2*alpha))*r*sin(th/2)
plot(r,res2, col = 2, type = 'b')
r[res < 10^(-10)]

th <- 2*pi*0.92
N <- 1000
par(new = TRUE)
res3 <- r^3*(2*pi-th)^3*exp(-r^2*(2*pi-th-sin(th))/(2*alpha))*r*sin(th/2)
res3 <- r^3*(2*pi-th)^3*(1-r^2*(2*pi-th-sin(th))/(2*alpha*N))^(N-3)*r*sin(th/2)
plot(r,res3, col = 3, type = 'b')



the <- seq(0, 2*pi, length.out = 100)
x <- seq(0, 2, length.out = 100)
area <- (2*pi-the + sin(the))/2
areaSquare <- pi*(1- x^2/2)
areaSquare2 <-pi/2*(2- x)^2
MAX <- max(area, areaSquare)
plot(1-cos(1/2*the), area, ylim = c(0,MAX), type = 'l')
par(new = TRUE)
plot(x, areaSquare, ylim = c(0,MAX), type = 'l', col = 3)
par(new = TRUE)
plot(x, areaSquare2, ylim = c(0,MAX), type = 'l', col = 2)
par(new = TRUE)
plot(x, pi/2*(2-x), ylim = c(0,MAX), type = 'l', col = 4)



1-cos(the/2)



x <- seq(0, 1, length.out = 1000)

N <- 10
plot(x,(1-x/N)^(N-3))
par(new = TRUE)
plot(x,exp(-x/N), col =2)
plot(x,(1-x/N)^(N-3) - exp(-x*(N-3)/N))

max(abs((1-x/N)^(N-3) - exp(-x)))
max(abs((1-x/N)^(N-3) - exp(-x*(N-3)/N)))
max(abs((1-x/N)^(N-3) - exp(-x*(N-3)/N))/abs((1-x/N)^(N-3)))



###########################################################################
#####

Pn <- function(x,n)
{
  a <- (-1)^n*factorial(n)
  b <- (1+x^2)^((n+1)/2)
  c <- sin((n+1)/sqrt(1+x^2))
  return(a*c/b)
}

BoundaryDensity <- function(alpha, r)
{
  R <- alpha*r^2
  u1 <- Pn(R,3)*(1+exp(-R*pi))
  u2 <- exp(-R*pi)*(-pi^3*Pn(R,0) + 3*pi^2 * Pn(R,1) - 3*pi *Pn(R,2))
  return(r^4 * (u1+u2))
}

#####
###########################################################################
###########################################################################
###########################################################################



N <- 300
Br <- 0.5

radii2 <- NULL
NB <- floor(1000000/N)*10

for(i in 1:NB)
{
  rho <- sqrt(runif(N))
  theta <- runif(N, 0, 2*pi)
  x <- rho * cos(theta)
  y <- rho * sin(theta)
  mat <- matrix(c(x,y),nrow = N, ncol = 2)
  mat
  data <- mat %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("x","y"))
  data <- data2D_pointSquareHole(N)
  res <- tri.mesh(x = data)
  #plot(res)
  sel <- (res$cclist[,1] + res$cclist[,3] > 1) & (res$cclist[,2] < 0.9) & (res$cclist[,2] > 0.1)
  radii <- res$cclist[sel ,3]
  radii3 <- radii[radii < Br]
  radii2 <- c(radii2, radii3)
}
plot(data, asp = 1)


####################################################################################################

alpha <- N

h <- hist(radii2, breaks = 1000, xlim = c(0,Br))
par(new = TRUE)
r <- seq(0, Br, length.out = 1000)
#res <- (1-r)*(r)^3 * (1-(r)^2)^(N-3)
#plot(r,res, type = 'l',xlim = c(0,Br)) # small N effet de bord
#par(new = TRUE)
#res <- -BoundaryDensity(alpha ,r)
#plot(r,res, type = 'l',xlim = c(0,Br), col = 2, lwd = 3) # small N effet de bord
#par(new = TRUE)
#res3 <- r^3*exp(-alpha*r^2*pi)
#plot(r,res3, col = 1, type = 'l',xlim = c(0,Br))


densityB <- function(theta, r) exp(-alpha*r^2*(2*pi - theta + sin(theta))/2) * sin(theta/2)^2*r^3*(2*pi-theta)^4

densityB <- function(theta, r) exp(-alpha*r^2*(2*pi - theta + sin(theta))/2) * sin(theta/2)^2*r^(3)*(2*pi-theta)^4
#densityB <- function(theta, r) exp(-alpha*r^(2)*(2*pi - theta + sin(theta))/2) * sin(theta/2)^2*r^(3.5)*(2*pi-theta)^4

#r^4*(2*pi-theta)^3*sin(theta/2)


resExact <- rep(0, length(r))

for(i in 1:length(r))
{
  rad <- r[i]
  densityBr <- function(theta) densityB(theta, rad)
  resExact[i] <- (integrate(densityBr, lower = 0, upper = 2*pi))[[1]]
}

par(new = TRUE)
plot(r,resExact, col = 4, type = 'l',xlim = c(0,Br), lwd = 3)


#plot(r[-(1:20)],(h$counts/resExact)[-(1:20)], col = 4, type = 'l',xlim = c(0,Br), lwd = 3)


#plot(th, 2*pi - th + sin(th))

