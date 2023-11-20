
#' data2D_pointCircleGap
#'
#' @description Function generating a tibble with 2 columns named (x,y) with 2D data forming on a circle with gap
#' @param n Overall number of points to draw
#' @param gap size of the gap
#' @return tibble with 2 columns (x,y) and n rows
#' @examples
#' data2D_pointCircleGap(1000, 0.1)
data2D_pointCircleGap <- function(n, gap)
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
