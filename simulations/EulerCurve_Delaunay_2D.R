

N <- 2000
data <- data2D_pointTwoCirclesMerged(N,overlap = 0.025)
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
plot(log(r), 100*(yNEW == 0)-99, col= 3, lwd = 2, type = 'l', ylim = yylim) #green 1 hole
par(new = TRUE)
plot(log(r), 100*(yNEW == -1)-99, col= 2, lwd = 2, type = 'l', ylim = yylim) #red 2 holes

#green 1 hole
#red 2 holes

#########




N <- 2000
data <- data2D_pointSquareHole(N, Hole_Relative_length = 0.09)
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
############################################################


