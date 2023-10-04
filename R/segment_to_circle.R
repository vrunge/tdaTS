

#' segment_to_circle
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a segment closing to a circle at the change-point location.
#' @param n Overall number of points to draw
#' @param change Relative location of the change (example: 0.5 = at the middle of the data, time goes from 0 to 1 with this function)
#' @param time_sampling unif (uniform continuous time sampling) or discrete (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option sampling = "discrete")
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' segment_to_circle()
segment_to_circle <- function(n = 1000,
                              change = 0.5,
                              time_sampling = "unif",
                              nb_levels = 20)
{
  #####
  ##### inner function start
  #####
  generator <- function(time, change)
  {
    pos <- runif(1, min = -pi, max = pi) #choosing uniformly a point on the segment/circle
    if(time < change)  ### in the move from segment to circle
    {
      time_scale <- time/change ### time scaling
      a <- cos(pi/2 * time_scale)
      b <- sin(pi/2 * time_scale)
      x <- pos * a + b * cos(pos - pi/2)
      y <- b * (1 + sin(pos - pi/2))
      return(c(time, x, y))
    }
    if(time >= change) ### point on a unit circle
    {
      x <- cos(pos - pi/2)
      y <- 1 + sin(pos - pi/2)
      return(c(time, x, y))
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


