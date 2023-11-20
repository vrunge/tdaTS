
#' data2D_pointEllipseMissingArc
#'
#' @description Function generating a tibble with 2 columns named (x,y) with 2D data forming on a circle with gap (a missing arc)
#' @param n Overall number of points to draw (around the circle of radius 1)
#' @param gap size of the missing arc as a percent of the global arc length (2 pi)
#' @param sdNoise additional Gaussian noise
#' @param a coefficient in front of cos(t)
#' @param b coefficient in front of sin(t)
#' @param gapStart uniform values between gapStart and 2*pi - gap + gapStart for the uniform t angles
#' @return tibble with 2 columns (x,y) and n rows
#' @examples
#' data2D_pointEllipseMissingArc(1000, 0.1)
data2D_pointEllipseMissingArc <- function(n = 1000, gap = 0.1, sdNoise = 0.1, a = 1, b = 1, gapStart = 0)
{
  gap <- 2 * pi * gap
  t_values <- runif(n, min = 0, max = 2 * pi - gap) + gapStart
  x <- a*cos(t_values) + rnorm(n = n, mean = 0, sd = sdNoise)
  y <- b*sin(t_values) + rnorm(n = n, mean = 0, sd = sdNoise)

  res <- matrix(c(x,y), nrow = n, byrow = FALSE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("x","y"))
  return(res)
}
