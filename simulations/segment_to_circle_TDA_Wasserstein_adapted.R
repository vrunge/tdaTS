
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


######## plot the data

resScale <- rbind(res,c(0,-pi,pi),c(0,pi,pi),
                  c(0, pi,-pi),c(0,-pi,-pi)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale,
        x=resScale$x,
        y=resScale$y,
        z=resScale$time,
        type="scatter3d",
        mode = "markers",
        color=resScale$time)



################################################
######## persistence diagrams
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

  diagramAndNB <- cbind(k, DiagAlphaCmplx$diagram)
  All_PD <- rbind(All_PD, diagramAndNB)
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



All_PD <- remove_noisy_points(All_PD, v = th)
dim(All_PD)

tau <- 5

PD1 <- All_PD[All_PD$time < tau,-1]
PD2 <- All_PD[All_PD$time >= tau,-1]
dim(PD1)
dim(PD2)
points <- proj_Points(PD1, v = th, h = 1)
PD2 <- rbind(PD2, points[,-1])

#compare
PD1
PD2


################################################
######## computing the Wasserstein distance
################################################








