
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

All_Persistence_diagrams <- function(level, n, data)
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



##################################################################################################
######## removing noise points
################################################

All_PD <- remove_noisy_points(All_PD, infinity = FALSE) #removing infinity points

boundsX <- c(0, max(c(1,All_PD$Birth)))
boundsY <- c(0, max(c(1,All_PD$Death)))

### INITIAL DATA
plot(All_PD[,-(1:2)], xlim = boundsX, ylim = boundsY, asp = 1, pch = All_PD[,2])
abline(a=0,b=1, col = 3, lwd = 2)
abline(v = 0, h = 0, col = 1, lwd = 2)

nb <- n/level
maxDistance <- (log(nb) -digamma(1))/nb
th <- maxDistance*2*pi/2 #length circle = 2pi


myV <- 0.5 ####### VERTICAL THRESHOLD
myH <- 0.2 ####### HORIZONTAL THRESHOLD
All_PD <- remove_noisy_points(All_PD, v = myV, h = myH)
dim(All_PD)

### DATA after thresholding
plot(All_PD[,-(1:2)], xlim = boundsX, ylim = boundsY, asp = 1, pch = All_PD[,2])
abline(a = 0, b = 1, v = myV, h = myH, col = 3, lwd = 2)
abline(v = 0, h = 0, col = 1, lwd = 2)


################################################
######## adding projective Points
################################################

tau <- 15

PD1 <- All_PD[All_PD$time < tau,]
PD2 <- All_PD[All_PD$time >= tau,]

dim(PD1)
dim(PD2)
points <- proj_Points(PD1, v = myV, h = myH)

### LEFT DATA
plot(PD1[,-(1:2)], xlim = boundsX, ylim = boundsY, asp = 1, pch = PD1[,2])
abline(a = 0, b = 1, v = myV, h = myH, col = 3, lwd = 2)
abline(v = 0, h = 0, col = 1, lwd = 2)


### LEFT DATA + PROJECTIONS
plot(PD1[,-(1:2)], xlim = boundsX, ylim = boundsY, asp = 1, pch = PD1[,2])
par(new = TRUE)
plot(points[,-(1:2)], xlim = boundsX, ylim = boundsY, col = 3, asp = 1)
abline(a = 0, b = 1, v = myV, h = myH, col = 3, lwd = 2)
abline(v = 0, h = 0, col = 1, lwd = 2)


PD2 <- rbind(PD2, points)

#compare
PD1
PD2


################################################
######## computing the Wasserstein distance
################################################








