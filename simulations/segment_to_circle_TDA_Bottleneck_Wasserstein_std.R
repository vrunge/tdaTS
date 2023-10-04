
library(TDA)
library(plotly)

################################################
######## data on regular time scale
################################################

n <- 1000
level <- 20 #plots ok for level = 20

res <- segment_to_circle(n = n,
                         change = 0.5,
                         time_sampling = "discrete",
                        nb_levels = level)

######## adding noise to the data
res[,2:3] <- res[,2:3] + rnorm(2*n, sd = 0.0)


resScale <- rbind(res,c(0,-pi,pi),c(0,pi,pi),
                  c(0, pi,-pi),c(0,-pi,-pi)) ### add 4 points in the corners for scaling x and y axes

plot_ly(resScale,
        x=resScale$x,
        y=resScale$y,
        z=resScale$t,
        type="scatter3d",
        mode = "markers",
        color=resScale$t)


################################################
######## persistence diagram dynamics
################################################
nb <- n/level
maxDistance <- (log(nb) -digamma(1))/nb

th <- maxDistance*2*pi/2 #length circle = 2pi

th_test <- NULL

par(mfrow = c(5, 4), mar=c(2,3,0,0), mgp=c(1.5,0.5,0))

for(k in 0:(level-1))
{
  tau <- n*k/level
  tau2 <- n*(k+1)/level
  res_slice <- res[(tau+1):tau2, ]
  X <- res_slice[,-1]

  DiagAlphaCmplx <- alphaComplexDiag(
                      X = X,
                      library = c("GUDHI", "Dionysus"),
                      location = TRUE,
                      printProgress = TRUE)

  plot(DiagAlphaCmplx$diagram, col = 1 + cumsum(DiagAlphaCmplx[["diagram"]][, 1]))
  abline(v = th)

  ### under the threshold test (TRUE OR FALSE)
  id <- which(DiagAlphaCmplx$diagram[,1] == 1)[1]
  value <- DiagAlphaCmplx$diagram[id,2] < th
  if(is.na(id)){th_test <- c(th_test, FALSE)}
  else{th_test <- c(th_test, unname(value))}
}

par(mfrow = c(1, 1))

th_test

sum(th_test[1:(level/2)])
sum(th_test[(level/2 + 1):level])

################################################
######## plot the data slice be slice
################################################

par(mfrow = c(5, 4))
for(k in 0:(level-1))
{
  tau <- n*k/level
  tau2 <- n*(k+1)/level
  res_slice <- res[(tau+1):tau2,]
  X <- res_slice[,-1]

  DiagAlphaCmplx <- alphaComplexDiag(
    X = X,
    library = c("GUDHI", "Dionysus"),
    location = TRUE,
    printProgress = FALSE)
  plot(X, col = 1)
  one <- which(DiagAlphaCmplx[["diagram"]][, 1] == 1)
  for (i in seq(along = one))
  {
    for (j in seq_len(dim(DiagAlphaCmplx[["cycleLocation"]][[one[i]]])[1]))
    {
      lines(DiagAlphaCmplx[["cycleLocation"]][[one[i]]][j, , ] + rnorm(n = 4,sd = 0.01), pch = 19, cex = 1, col = i + 1)
    }
  }
}
par(mfrow = c(1, 1))


################################################
######## computing the distances
################################################


all_PD <- NULL

for(k in 0:(level-1))
{
  tau <- n*k/level
  tau2 <- n*(k+1)/level
  res_slice <- res[(tau+1):tau2,]
  X <- res_slice[,-1] ### removing the time (the same here for data in the same slice)
  temp <- alphaComplexDiag(
    X = X,
    library = c("GUDHI", "Dionysus"),
    location = TRUE,
    printProgress = FALSE)
  all_PD <- do.call(c, list(all_PD, temp))
}
all_PD

D_bottleneck <- matrix(0, nrow = level, ncol = level)
D_wasserstein <- matrix(0, nrow = level, ncol = level)

for(i in 1:level)
{
  for(j in 1:level)
  {
    print(c(i,j))
    u <- all_PD[[1 + 4*(i-1)]]
    v <- all_PD[[1 + 4*(j-1)]]
    D_bottleneck[i,j] <- TDA::bottleneck(u, v, dimension = 1)
    D_wasserstein[i,j] <- TDA::wasserstein(u, v, p = 2, dimension = 1)
  }
}

library(fields)
D_bottleneck <- t(D_bottleneck)
D_wasserstein <- t(D_wasserstein)
image.plot(D_bottleneck[,nrow(D_bottleneck):1], axes = F, col = grey(seq(0,1, length = 256)))
axis(1, at=seq(0,1,length.out = level), labels= 1:level)
axis(2, at=seq(0,1,length.out = level), labels= level:1)
segments(0,0.5,1,0.5, col = 2, lwd = 4)
segments(0.5,0,0.5,1, col = 2, lwd = 4)
image.plot(D_wasserstein[,nrow(D_wasserstein):1], axes = F,  col = grey(seq(0,1, length = 256)))
axis(1, at=seq(0,1,length.out = level), labels= 1:level)
axis(2, at=seq(0,1,length.out = level), labels= level:1)
segments(0,0.5,1,0.5, col = 2, lwd = 4)
segments(0.5,0,0.5,1, col = 2, lwd = 4)

#change between levels 10 and 11
# problem with these 2 distances
# we need to remove point from H1 greater than a threshold for birth
# we see the H1 point for the circle moving


