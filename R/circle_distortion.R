
#' circle_distortion
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a circle transformed into an ellipse over time. (Using the right distance between Persistence Diagrams, this example should be change-point free)
#' @param n Overall number of points to draw
#' @param X_rate x axis distortion over time
#' @param Y_rate y axis distortion over time
#' @param time_sampling unif (uniform continuous time sampling) or discrete (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option sampling = "discrete")
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' circle_distortion()
circle_distortion <- function(n = 1000,
                              X_rate = 2,
                              Y_rate = 2,
                              time_sampling = "unif",
                              nb_levels = 20)
{
  #####
  ##### inner function start
  #####
  generator <- function(time)
  {
    pos <- runif(1, min = -pi, max = pi) #choosing uniformly a point on the segment/circle
    x <- cos(pos - pi/2)*(X_rate*time + 1 - time)
    y <- sin(pos - pi/2)*(Y_rate*time + 1 - time)
    return(c(time, x, y))
  }
  #####
  ##### inner function end
  #####

  if(time_sampling == "unif")
  {
    times <- sort(runif(n))
    l <- lapply(times, generator)
  }
  if(time_sampling == "discrete")
  {
    v <- 0:(n-1) %/% (n/nb_levels)
    times <- v/(nb_levels)
    l <- lapply(times, generator)
  }

  res <- matrix(unlist(l), nrow = length(l), byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("t","x","y"))

  return(res)
}


