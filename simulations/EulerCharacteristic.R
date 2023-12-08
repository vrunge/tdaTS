



nb <- 1000
data <- data2D_pointEllipseMissingArc(nb, gap = 0.3, sdNoise = 0.5, a = 3, b= 3)
data <- data2D_pointTwoCirclesMerged(nb, overlap = 0.1, sdNoise = 0.05)
data <- data2D_pointSquareHole(nb, Hole_Relative_length = 0.2)



plot(data, asp = 1)


################################################################################


filtre <- alphaComplexFiltration(X = data)


new_counts <- function(filtre, i)
{
  ret <- c(0,0,0)
  r <- unique(filtre$values)
  liste <- filtre$cmplx[filtre$values == r[i]]
  for(i in 1:length(liste))
    ret[length(liste[[i]])] <- ret[length(liste[[i]])] + 1
  return(ret)
}


r <- unique(filtre$values)[-length( unique(filtre$values))]
res <- matrix(0,length(r)+1,3)
for(j in 1:(length(r)))
{
  res[j+1,] <-  res[j,] + new_counts(filtre, j)
}




x <- r
y <- res[-1,1]-res[-1,2]+res[-1,3]


plot(log(x), log(y+1), type = 'l')
sel <- y < 3
x2 <- x[sel]
y2 <- y[sel]
plot(log(x2),y2, type = 'l')




#z <- log(unique(filtre$values))
#hist(z,breaks = 100)
#library(ggpubr)
#ggqqplot(z)



y2 <- y[-length(y)]
x2 <- diff(x)

z <- min(y2):max(y2)

zl <- rep(0, 11)

for(i in 1:length(z))
{
  zl[i] <- sum(log(x2[y2 == z[i]]))
}

plot(z,zl, type = 'l')

z[which.min(zl)]

z


nb <- 100
data <- data2D_pointSquareHole(nb, Hole_Relative_length = 0.3)
pl <- myplots(data)
res <- EulerCurve(data)
plotCurve(res)



m <- length(res$y)
while(res$y[m] == 1){m <- m - 1}
m <- m + 1
y <- res$y[1:m]
x <- diff(res$x[1:(m+1)])
plot(-3:3, sapply(-3:3, function(u) sum(x[y == u])), type = 'b')











