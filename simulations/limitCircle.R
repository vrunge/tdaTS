

library(TDA)
library(tibble)

myplots <- function(data, complex = "alpha")
{
  if(complex == "alpha")
  {
    DiagAlphaCmplx <- alphaComplexDiag(X = data,
                                       library = c("GUDHI", "Dionysus"),
                                       location = TRUE)
  }
  if(complex == "rips")
  {
    DiagAlphaCmplx <- ripsDiag(X = data,
                               maxdimension = 1,
                               maxscale = 1,
                               dist = "euclidean",
                               library = c("GUDHI", "Dionysus"),
                               location = TRUE)
  }

  plot(DiagAlphaCmplx$diagram, barcode = TRUE)
  plot(data, col = 1,xaxt="n", yaxt="n",xlab="", ylab="", asp = 1)
  one <- which(DiagAlphaCmplx[["diagram"]][, 1] == 1)
  for (i in seq(along = one))
  {
    for (j in seq_len(dim(DiagAlphaCmplx[["cycleLocation"]][[one[i]]])[1]))
    {lines(DiagAlphaCmplx[["cycleLocation"]][[one[i]]][j, , ] + rnorm(n = 4,sd = 0.0),cex = 1, col = i + 1)}
  }
  return(DiagAlphaCmplx$diagram)
}


pointCircleGap <- function(n, gap)
{
  gap <- 2 * pi * gap
  t <- runif(n, min = 0, max = 2 * pi - gap)
  x <- cos(t)
  y <- sin(t)
  res <- matrix(c(x,y), nrow = n, byrow = FALSE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("x","y"))
  return(res)
}





nb <- 200
data <- pointCircleGap(nb, 0)
data$x <- data$x + rnorm(n= nb, mean = 0, sd = 0.1)
data$y <- data$y + rnorm(n = nb, mean = 0, sd = 0.1)
diag3 <- myplots(data)
sel <- diag3[,1] == 1
res3 <- diag3[sel,3]/diag3[sel,2]
hist(log(log(res3)) - mean(log(log(res3))), breaks = 200)


max(res3)







