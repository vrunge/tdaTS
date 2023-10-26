
#' circle_extinction
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a circle transformed into a segment by reduction of one of the semi-axis
#' @param n Overall number of points to draw
#' @param change Relative location of the change (example: \code{change = 0.5} = at the middle of the data, time goes from 0 to 1 with this function)
#' @param time_sampling \code{"unif"} (uniform continuous time sampling) or \code{"discrete"} (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option \code{sampling = "discrete"}). The n points are distributed uniformily (same amount by level when possible) on all the \code{nb_levels}.
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' circle_extinction()
circle_extinction <- function(n = 1000,
                              change = 0.5,
                              time_sampling = "unif",
                              nb_levels = 20)
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
      x <- cos(pos - pi/2)
      y <- sin(pos - pi/2)*(1 - time_scale)
      return(c(times, x, y))
    }
    if(times >= change) ### point on a unit circle
    {
      x <- cos(pos - pi/2)
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


