
#' data2D_pointSquareHole
#'
#' @description Function generating a tibble with 2 columns named (x,y) with 2D data forming in a 1by1 square with a hole in its center of size `Hole_Relative_length
#' @param n Overall number of points to draw
#' @param Hole_Relative_length size of the hole in the center of the square
#' @return tibble with 2 columns (x,y) and n rows
#' @examples
#' data2D_pointSquareHole(1000)
data2D_pointSquareHole <- function(n, Hole_Relative_length = 0)
{
  len <- sqrt(Hole_Relative_length)
  a <- (1-Hole_Relative_length)/2
  b <- (1+Hole_Relative_length)/2
  mat <- matrix(NA,nrow = 0, ncol = 2)
  while(nrow(mat) < n)
  {
    u <- runif(2)
    if(!(u[1] > a &  u[1] < b  & u[2] > a & u[2] < b)){mat <- rbind(mat, u)}

  }
  res <- mat %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("x","y"))
  return(res)
}
