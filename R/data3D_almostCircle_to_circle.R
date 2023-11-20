
#' data3D_almostCircle_to_circle
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a circle almost closed, closing at time step \cide{change) into a circle. The transformation is "isometric" as the length of the object stays the same over time (= 2 x pi) (only if no distortion with \code{X_Rate = 1} and \code{Y_Rate = 1})
#' @param n Overall number of points to draw
#' @param change Relative location of the change (example: \code{change = 0.5} = at the middle of the data, time goes from 0 to 1 with this function)
#' @param gap distance between the circle and the segment From 0 to 1. 0 = the circle, 1 = the segment
#' @param time_sampling \code{"unif"} (uniform continuous time sampling) or \code{"discrete"} (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option \code{sampling = "discrete"}). The n points are distributed uniformily (same amount by level when possible) on all the \code{nb_levels}.
#' @param X_rate regular-in-time multiplicative factor for the x axis (x-coordinates are multiplicated by \code{(X_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @param Y_rate regular-in-time multiplicative factor for the y axis (y-coordinates are multiplicated by \code{(Y_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' data3D_almostCircle_to_circle()
data3D_almostCircle_to_circle <- function(n = 1000,
                                          gap = 0.1,
                                          change = 0.5,
                                          time_sampling = "unif",
                                          nb_levels = 20,
                                          X_rate = 1,
                                          Y_rate = 1)
{
  #####
  ##### inner function start
  #####
  generator <- function(times, change, gap)
  {
    pos <- runif(1, min = -pi, max = pi) #choosing uniformly a point on the segment/circle
    if(times < change)  ### in the move from segment to circle
    {
      time_scale <- 1 - gap ### gap between 0 and 1 (gap = 0 the circle, gap = 1 the segment)
      a <- cos(pi/2 * time_scale)
      b <- sin(pi/2 * time_scale)
      x <- (pos * a + b * cos(pos - pi/2)) * (X_rate*times + 1 - times)
      y <- (b * (1 + sin(pos - pi/2))) * (Y_rate*times + 1 - times)
      return(c(times, x, y))
    }
    if(times >= change) ### point on a unit circle
    {
      x <- cos(pos - pi/2) * (X_rate*times + 1 - times)
      y <- (1 + sin(pos - pi/2)) * (Y_rate*times + 1 - times)
      return(c(times, x, y))
    }
  }
  #####
  ##### inner function end
  #####

  if(time_sampling == "unif")
  {
    times <- sort(runif(n))
    l <- lapply(times, generator, change = change, gap = gap)
  }
  if(time_sampling == "discrete")
  {
    v <- 0:(n-1) %/% (n/nb_levels)
    times <- v/(nb_levels)
    l <- lapply(times, generator, change = change, gap = gap)
  }

  res <- matrix(unlist(l), nrow = length(l), byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("t","x","y"))

  return(res)
}

