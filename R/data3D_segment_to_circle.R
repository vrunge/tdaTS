

#' data3D_segment_to_circle
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a segment closing itself continuously into a circle at the change-point location. The transformation is "isometric" as the length of the initial segment (= 2 x pi) at time t=0 is preserved at all time steps and the obtained circle has a circumference of length 2 x pi (only if no distortion with \code{X_Rate = 1} and \code{Y_Rate = 1})
#' @param n Overall number of points to draw
#' @param change Relative location of the change (example: \code{change = 0.5} = at the middle of the data, time goes from 0 to 1 with this function)
#' @param time_sampling \code{"unif"} (uniform continuous time sampling) or \code{"discrete"} (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option \code{sampling = "discrete"}). The n points are distributed uniformily (same amount by level when possible) on all the \code{nb_levels}.
#' @param X_rate regular-in-time multiplicative factor for the x axis (x-coordinates are multiplicated by \code{(X_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @param Y_rate regular-in-time multiplicative factor for the y axis (y-coordinates are multiplicated by \code{(Y_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' data3D_segment_to_circle()
data3D_segment_to_circle <- function(n = 1000,
                                     change = 0.5,
                                     time_sampling = "unif",
                                     nb_levels = 20,
                                     X_rate = 1,
                                     Y_rate = 1)
{
  #####
  ##### inner function start
  #####
  generator <- function(times, change)
  {
    pos <- runif(1, min = -pi, max = pi) #choosing uniformly a point on the segment/circle
    if(times < change)  ### in the move from segment to circle
    {
      time_scale <- times/change ### time scaling
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
    l <- lapply(times, generator, change = change)
  }
  if(time_sampling == "discrete")
  {
    v <- 0:(n-1) %/% (n/nb_levels)
    times <- v/(nb_levels)
    l <- lapply(times, generator, change = change)
  }

  res <- matrix(unlist(l), nrow = length(l), byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("t","x","y"))

  return(res)
}


