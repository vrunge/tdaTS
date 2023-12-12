

#' dataUniform
#'
#' @description Uniform data over 2 segments with segment lengths L1,L2, constant densities a, b and N data points
#' @param N number of points to generate
#' @param L1 segment length, interval 1
#' @param L2 segment length, interval 2
#' @param M distance between the intervals
#' @param a density level for interval 1
#' @param b density level for interval 2
#' @examples
#' dataUniform(1000)
dataUniform <- function(N, L1 = 1, L2 = 1, M = 1, a = 1, b = 1)
{
  sel <- sample(c(1,0), size = N, replace = TRUE, prob = c(a,b))
  res <- runif(N,min = 0, max = L1)*sel + runif(N,min = L1 + M, max = L1 + M + L2)*(1-sel)
  return(res)
}




#' EulerCPC_approx
#'
#' @description Approximate value of the Euler curve for 1D Uniform data (with 2 segments)
#' @param r evaluation point (radius r)
#' @param N number of points
#' @param N1 number of points in segment 1
#' @param N2 number of points in segment 2
#' @param L1 segment length, interval 1
#' @param L2 segment length, interval 2
#' @param M distance between the intervals
EulerCPC_approx <- function(r, N, N1, N2, L1, L2, M)
{
  res <- N
  res <- res - (N1-1)*(1-(1-pmin(2*r/L1,1))^N1)
  res <- res - (N2-1)*(1-(1-pmin(2*r/L2,1))^N2)

  y <- (2*r - M)/(L1+L2)
  r1 <- - (1-y)^(N1+N2+1)
  r2 <- -(N1+N2+1) * y*(1-y)^(N1+N2)
  res <- res - (1+r1+r2)*((2*r >= M))
  return(res)
}





#' EulerCPC_exact
#'
#' @description Exact value of the Euler curve for 1D Uniform data (with 2 segments)
#' @param r evaluation point (radius r)
#' @param N number of points
#' @param N1 number of points in segment 1
#' @param N2 number of points in segment 2
#' @param L1 segment length, interval 1
#' @param L2 segment length, interval 2
#' @param M distance between the intervals
EulerCPC_exact <- function(r, N, N1, N2, L1, L2, M)
{
  res <- N
  res <- res - (N1-1)*(1-(1-pmin(2*r/L1,1))^N1)
  res <- res - (N2-1)*(1-(1-pmin(2*r/L2,1))^N2)

  g <- N1*N2/(L1^N1*L2^N2)*(L1+L2)^(N1+N2)
  f_l <- function(x) pmin(1,L2/(L1+L2-x*(L1+L2)))
  f_r <- function(x) pmax(0,(L2-x*(L1+L2))/(L1+L2-x*(L1+L2)))

  f_ibeta <- function(x) g*(Ibeta(f_l(x),N2,N1) - Ibeta(f_r(x),N2,N1))*(1-x)^(N1+N2-1)
  integ <- sapply(r, function(r) {u <- integrate(f_ibeta,0,min((2*r-M)/(L1+L2),1)); u[[1]]})
  res <- res - integ*((2*r >= M))
  return(res)
}



#' EulerCPC_VAR_exact
#'
#' @description Exact value of the Variance of the Euler curve for 1D Uniform data (with 1 segment)
#' @param r evaluation point (radius r)
#' @param N number of points
#' @param L segment length
EulerCPC_VAR_exact <- function(r, N, L)
{
  A <- (N-1)*(1-pmin(2*r/L,1))^N*(1 -(1-pmin(2*r/L,1))^N)
  B <- -(N-1)*(N-2)*(1 -(1-pmin(2*r/L,1))^N)^2
  C <- (N-1)*(N-2)*(1 - 2*(1-pmin(2*r/L,1))^N +(1 - pmin(2*r/L,1) - pmin(2*r/L,1-(2*r/L)))^N)
  return(A+B+C)
}


#' EulerCPC_VAR_approx
#'
#' @description Approximate value of the Variance of the Euler curve for 1D Uniform data (with 1 segment)
#' @param r evaluation point (radius r)
#' @param N number of points
#' @param L segment length
EulerCPC_VAR_approx <- function(r, N, L)
{
  A <- (1-pmin(2*r/L,1))^N*(1 -(1-pmin(2*r/L,1))^N)
  C <- (N-2)*((1-pmin(4*r/L,1))^N - (1-pmin(2*r/L,1))^(2*N))
  return((N-1)*(A+C))
}








