


############################################################
### examples of Euler curves
############################################################


N <- 2000
data <- data2D_pointTwoCirclesMerged(N,overlap = 0.02)
res <- tri.mesh(x = data)
plot(res)


triangles <- res$trlist[,7:9]
radii <- res$cclist[,3]

nb <- 10000
r <- seq(0,1, length.out = nb)

yNEW <- rep(0, nb)
for(i in 1:nb)
{
  sel <- radii <= r[i]
  nbTriangles <- sum(sel)
  nbEdges <- length(unique(as.vector(triangles[sel,])))
  yNEW[i] <- N - nbEdges + nbTriangles
}

yylim <- c(0, max(yNEW))
plot(log(r), yNEW, type = 'l', ylim = yylim, lwd = 2)
par(new = TRUE)
plot(log(r), 100*(yNEW == 0)-100, col= 3, lwd = 2, type = 'l', ylim = yylim) #green 1 hole
par(new = TRUE)
plot(log(r), 100*(yNEW == -1)-101, col= 2, lwd = 2, type = 'l', ylim = yylim) #red 2 holes

#green 1 hole
#red 2 holes

#########




N <- 2000
data <- data2D_pointSquareHole(N, Hole_Relative_length = 0.1)
res <- tri.mesh(x = data)
plot(res)


triangles <- res$trlist[,7:9]
radii <- res$cclist[,3]

nb <- 10000
r <- seq(0,1, length.out = nb)

yNEW <- rep(0, nb)
for(i in 1:nb)
{
  sel <- radii <= r[i]
  nbTriangles <- sum(sel)
  nbEdges <- length(unique(as.vector(triangles[sel,])))
  yNEW[i] <- N - nbEdges + nbTriangles
}

yylim <- c(0, max(yNEW))
plot(log(r), yNEW, type = 'l', ylim = yylim, lwd = 2)
par(new = TRUE)
plot(log(r), 100*(yNEW == 1)-99, col= 3, lwd = 2, type = 'l', ylim = yylim) #green 0 hole
par(new = TRUE)
plot(log(r), 100*(yNEW == 0)-99, col= 2, lwd = 2, type = 'l', ylim = yylim) #red 1 holes

#green 0 hole
#red 1 hole


############################################################
### radius density (internal density)
############################################################


N <- 500
Br <- min(200/N,1)

radii2 <- NULL
NB <- floor(100000/N)

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
res <- tri.mesh(x = data)
#plot(res)
radii <- res$cclist[,3]
radii3 <- radii[radii <Br]
radii2 <- c(radii2, radii3)
}
plot(mat, asp = 1)



hist(radii2, breaks = 100,xlim = c(0,Br))
par(new = TRUE)
r <- seq(0,min(200/N,1), length.out = 1000)
res <- (1-r)*(r)^3 * (1-(r)^2)^(N-3)
plot(r,res, type = 'l',xlim = c(0,Br)) # small N effet de bord



#### CDF CDF

nbR <- sort(radii2)
histoCDF <- NULL
for(i in 1:length(r))
{
  histoCDF <- c(histoCDF, sum(nbR < r[i]))
}

plot(r, histoCDF/max(histoCDF), col = 1, type = 'l', lwd = 3, ylim = c(0, 1))
par(new = TRUE)
library(pracma)
cdf <- (exp(-((3 + N)* r^2))* (4* (3 + N) *r^3 - 4* (3 + N)* r^2  + 6 *r + 4 *(-1 + exp(((3 + N) *r^2)))))/(8* (3 + N)^2) - (3* sqrt(pi)* erf((sqrt(3 + N) *r)))/(8* (3 + N)^(5/2))
plot(r, cdf/max(cdf), type = 'l', col = 3, lwd = 3, ylim = c(0, 1))




### ### ### ### ### ### ###
### delete effet de bord
### ### ### ### ### ### ###

R <- 1.4
N <- floor(R^2*N)

radii2 <- NULL

for(i in 1:NB)
{
  rho <- R * sqrt(runif(N))
  theta <- runif(N, 0, 2*pi)
  x <- rho * cos(theta)
  y <- rho * sin(theta)
  mat <- matrix(c(x,y),nrow = N, ncol = 2)
  mat
  data <- mat %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("x","y"))
  res <- tri.mesh(x = data)

  #plot(res)
  temp <- res$cclist
  sel <- sqrt(temp[,1]^2 + temp[,2]^2) + temp[,3] < 1
  radii <- res$cclist[sel,3]
  radii3 <- radii[radii <Br]
  radii2 <- c(radii2, radii3)
}



hist(radii2, breaks = 100,xlim = c(0,Br))
par(new = TRUE)
r <- seq(0,Br, length.out = 1000)
res <- (1-r/R)*(r/R)^3*(1-(r/R)^2)^(N+3) #ok also with (r/R)^3*(1-(r/R)^2)^(N+3)
plot(r,res, type = 'l',xlim = c(0,Br)) # small N effet de bord

par(new = TRUE)
res2 <- (1-r/R)*(r/R)^3*exp(-(r/R)^2*(N+3))
plot(r,res2, type = 'l',xlim = c(0,Br), col = 2) # small N effet de bord

res/res2


#### CDF CDF

nbR <- sort(radii2)
histoCDF <- NULL
for(i in 1:length(r))
{
  histoCDF <- c(histoCDF, sum(nbR < r[i]))
}

plot(r, histoCDF/max(histoCDF), col = 1, type = 'l', lwd = 3, ylim = c(0, 1))
par(new = TRUE)
library(pracma)
cdf <- (exp(-((3 + N)* r^2)/R^2)* (4* (3 + N) *r^3 - 4* (3 + N)* r^2 *R + 6 *r* R^2 + 4 *(-1 + exp(((3 + N) *r^2)/R^2))* R^3))/(8* (3 + N)^2* R^2) - (3* sqrt(pi)* R* erf((sqrt(3 + N) *r)/R))/(8* (3 + N)^(5/2))
plot(r, cdf/max(cdf), type = 'l', col = 3, lwd = 3, ylim = c(0, 1))

