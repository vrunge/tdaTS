
library(TDA)
library(plotly)
library(transport)

################################################
######## data on regular time scale
################################################


n <- 400
level <- 20 #plots ok for level = 20

res <- segment_to_circle(nb = n,
                         change = 0.5,
                         sampling = "discrete",
                         level = level)

######## adding noise to the data
res[,2:3] <- res[,2:3] + rnorm(2*n, sd = 0.0)


################################################
######## persitence diagrams
################################################

All_PD <- NULL
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
    printProgress = TRUE)

  diagram <- cbind(k, DiagAlphaCmplx$diagram)
  All_PD <- rbind(All_PD, diagram)
}
colnames(All_PD)[1] <- "time"
All_PD <- as.data.frame(All_PD)
dim(All_PD)


################################################
######## computing projective points on boundaries
################################################

nb <- n/level
maxDistance <- (log(nb) -digamma(1))/nb
th <- maxDistance*2*pi/2 #length circle = 2pi

boundary_points <- function(PD, v = 0, h = 0)
{
  n <- nrow(PD)
  newPoints <- NULL

  #### points on the horizontal boundary
  if(h > 0)
  {
    for(i in 1:n)
    {
      if(All_PD[i,4] < h){newPoints <- rbind(newPoints, c(All_PD[i,1:3], h))} #create
    }
  }

  #### points on the vertical boundary
  if(v > 0)
  {
    for(i in 1:n)
    {
      if((All_PD[i,3] < v) & (All_PD[i,4] > v))
        {newPoints <- rbind(newPoints, c(All_PD[i,1:2], v, All_PD[i,4]))}
    }
  }

  #### points on the diagonal
  for(i in 1:n)
  {
    m <- mean(unlist(All_PD[i,3:4]))
    newPoints <- rbind(newPoints, c(All_PD[i,1:2], m, m))
  }

  colnames(newPoints) <- c("time", "dimension", "Birth", "Death")
  return(newPoints)
}



remove_noisy_points <- function(PD, v = 0, h = 0)
{
  n <- nrow(PD)

  #### points on the horizontal boundary
  if(h > 0)
  {
    for(i in 1:n)
    {
      if(PD[i,4] >= h){PD[i,1] <- -1} #remove
    }
  }

  #### points on the vertical boundary
  if(v > 0)
  {
    for(i in 1:n)
    {
      if(PD[i,3] >= v){PD[i,1] <- -1}
    }
  }

  PD <- PD[PD$time >= 0,]
  return(PD)
}



All_PD <- remove_noisy_points(All_PD, v = th)
dim(All_PD)

tau <- 5

PD1 <- All_PD[All_PD$time < tau,-1]
PD2 <- All_PD[All_PD$time >= tau,-1]
dim(PD1)
dim(PD2)
points <- boundary_points(PD1, v = th, h = 1)
PD2 <- rbind(PD2, points[,-1])


PD1
PD2


################################################
######## computing the Wasserstein distance
################################################








