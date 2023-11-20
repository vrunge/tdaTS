
#' data2D_pointTwoCirclesMerged
#'
#' @description Function generating a tibble with 2 columns named (x,y) with 2D data on two overlapping circles (no point in the overlap)
#' @param n Overall number of points to draw (two circle of radius 1 with overlap)
#' @param overlap we have two circle of radius 1, the first located in (0,0), the second in (0, 2-overlap)
#' @param sdNoise additional Gaussian noise
#' @return tibble with 2 columns (x,y) and n rows
#' @examples
#' data2D_pointTwoCirclesMerged(1000, 0.1)
data2D_pointTwoCirclesMerged <- function(n = 1000, overlap = 0.1, sdNoise = 0.05)
{

  bound <- acos((2-overlap)/2)

  t_values1 <- runif(n, min = bound, max = 2 * pi - bound)
  t_values2 <- runif(n, min = bound, max = 2 * pi - bound) + pi

  x1 <- cos(t_values1) + rnorm(n = n, mean = 0, sd = sdNoise)
  y1 <- sin(t_values1) + rnorm(n = n, mean = 0, sd = sdNoise)
  x2 <- 2 -overlap + cos(t_values2) + rnorm(n = n, mean = 0, sd = sdNoise)
  y2 <- sin(t_values2) + rnorm(n = n, mean = 0, sd = sdNoise)

  sel <- rbinom(n, 1, 0.5)
  x <- c(x1[sel == 1], x2[sel == 0])
  y <- c(y1[sel == 1], y2[sel == 0])
  res <- matrix(c(x,y), nrow = n, byrow = FALSE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("x","y"))
  return(res)
}

