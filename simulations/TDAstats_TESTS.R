


data <- TWO_D_pointSquareHole(30000, 0)
plot(data)

DiagAlphaCmplx <- ripsDiag(X = data,
                           maxdimension = 1,
                           maxscale = 1,
                           dist = "euclidean",
                           library = c(" Dionysus"),
                           location = TRUE,
                           printProgress = TRUE)

DiagAlphaCmplx <- alphaComplexDiag(X = data,
                                   library = c("GUDHI"),
                                   location = TRUE)

diagALPHA <- DiagAlphaCmplx$diagram
dim(diagALPHA)
sel <- diagALPHA[,1] == 1
sum(sel)
res <- diagALPHA[sel,3]/diagALPHA[sel,2]
normalization <- 0.5
gumbel <- normalization*(log(log(res)) - mean(log(log(res)))) + digamma(1)
max(gumbel)
hist(gumbel, breaks = 200, probability = TRUE,xlim = c(min(gumbel), max(gumbel)+1))

x2 <- seq(min(gumbel), max(gumbel)+1, length = 1000)
fun <- exp(x2 - exp(x2))
lines(x2, fun, col = 2, lwd = 2)


#######################################################

library(TDAstats)

pvalue <- rep(0,100)
gap <- 0.3
nb <- 300
for(i in 1:100)
{
  data <- TWO_D_pointSquareHole(nb, gap)
  diagALPHA <- calculate_homology(data, dim = 1)
  sel <- diagALPHA[,1] == 1
  res <- diagALPHA[sel,3]/diagALPHA[sel,2]
  gumbel <- log(log(res)) - mean(log(log(res))) + digamma(1)

  pvalue[i] <- exp(-exp(max(gumbel)))
}

hist(pvalue, breaks = 20, xlim=c(0,1))
abline(v = 0.05)

sum(pvalue < 0.005) #presence du trou



#######################################################

library(TDAstats)


data <- TWO_D_pointSquareHole(2000, 0.4)
plot(data)
diagALPHA <- calculate_homology(data, dim = 1)
sel <- diagALPHA[,1] == 1
res <- diagALPHA[sel,3]/diagALPHA[sel,2]
gumbel <- log(log(res)) - mean(log(log(res))) + digamma(1)
hist(gumbel, breaks = 50, probability = TRUE)

x2 <- seq(min(gumbel), max(gumbel)+0.5, length = 1000)
fun <- exp(x2 - exp(x2))
lines(x2, fun, col = 2, lwd = 2)
exp(-exp(max(gumbel)))



pvalues <- rep(0,600)
index <- (floor(0:599/100)+1)

for(i in 1:600)
{
  print(i)
  j <- index[i]
data <- TWO_D_pointCircleGap(j*100, 0.1)
data$x <- data$x + rnorm(n= j*100, mean = 0, sd = 0.05)
data$y <- data$y + rnorm(n = j*100, mean = 0, sd = 0.05)
#plot(data)
diagALPHA <- calculate_homology(data, dim = 1)
sel <- diagALPHA[,1] == 1
res <- diagALPHA[sel,3]/diagALPHA[sel,2]
gumbel <- log(log(res)) - mean(log(log(res))) + digamma(1)
#hist(gumbel, breaks = 50, probability = TRUE)

#x2 <- seq(min(gumbel), max(gumbel)+0.5, length = 1000)
#fun <- exp(x2 - exp(x2))
#lines(x2, fun, col = 2, lwd = 2)
pvalues[i] <- exp(-exp(max(gumbel)))
}


plot(pvalues, type = 'l')
plot(log(pvalues), type = 'l')




