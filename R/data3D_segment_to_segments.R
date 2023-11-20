

#' data3D_segment_to_segments
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a segment cut into two smaller segments over time. The transformation is "isometric" as the length of the initial segment (= 1) at time t=0 is preserved at all time steps with \code{X_Rate = 1}
#' @param n Overall number of points to draw
#' @param change Relative location of the change (example: \code{change = 0.5} = at the middle of the data, time goes from 0 to 1 with this function)
#' @param gap Final gap between the 2 segments
#' @param time_sampling \code{"unif"} (uniform continuous time sampling) or \code{"discrete"} (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option \code{sampling = "discrete"}). The n points are distributed uniformily (same amount by level when possible) on all the \code{nb_levels}.
#' @param X_rate regular-in-time multiplicative factor for the x axis (x-coordinates are multiplicated by \code{(X_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' data3D_segment_to_segments()
data3D_segment_to_segments <- function(n = 1000,
                                       change = 0.5,
                                       time_sampling = "unif",
                                       gap = 0.5,
                                       nb_levels = 20,
                                       X_rate = 1)
{
  #####
  ##### inner function start
  #####
  generator <- function(times, change, gap)
  {
    pos <- runif(1, min = 0, max = 1) #choosing uniformly a point on the segment
    if(times < change)  ### in the move from segment to circle
    {
      x <- pos * (X_rate*times + 1 - times)
      y <- 0
      return(c(times, x, y))
    }
    if(times >= change) ### point on a unit circle
    {
      time_scale <- (times-change)/(1-change) ### time scaling

      x <- (pos + time_scale*gap*(pos >= 0.5)) * (X_rate*times + 1 - times)
      y <- 0
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
