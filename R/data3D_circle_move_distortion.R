
#' data3D_circle_move_distortion
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a circle transformed into an ellipse over time with rotation and shift of the center position. (Using the right distance between Persistence Diagrams, this example should be change-point free)
#' @param n Overall number of points to draw
#' @param final_position the x-y coordinate of the barycenter of the final ellipse at \code{time = 1}. The figure moves linearly with time to this position.
#' @param rotation the number of rotations of the final figure, which is rotated linearly with time (given in radians as \code{rotation x 2 pi}). If equal to 0.5, the rotation angle is pi for example.
#' @param time_sampling \code{"unif"} (uniform continuous time sampling) or \code{"discrete"} (data are located at discrete regular time steps)
#' @param nb_levels number of time steps (for option \code{sampling = "discrete"}). The n points are distributed uniformily (same amount by level when possible) on all the \code{nb_levels}.
#' @param X_rate regular-in-time multiplicative factor for the x axis (x-coordinates are multiplicated by \code{(X_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @param Y_rate regular-in-time multiplicative factor for the y axis (y-coordinates are multiplicated by \code{(Y_rate*time + 1 - time)} where \code{time} goes from 0 to 1)
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' data3D_circle_move_distortion()
data3D_circle_move_distortion <- function(n = 1000,
                                          final_position = c(2,3),
                                          rotation = 0.5,
                                          time_sampling = "unif",
                                          nb_levels = 20,
                                          X_rate = 2,
                                          Y_rate = 0.3)
{
  #####
  ##### inner function start
  #####
  generator <- function(times)
  {
    pos <- runif(1, min = -pi, max = pi) #choosing uniformly a point on the segment/circle
    xx <- cos(pos - pi/2)*(X_rate*times + 1 - times)
    yy <- sin(pos - pi/2)*(Y_rate*times + 1 - times)
    angle <- rotation * 2 * pi * times
    x <- xx * cos(angle) - yy * sin(angle)
    y <- xx * sin(angle) + yy * cos(angle)
    x <- x + final_position[1]*times
    y <- y + final_position[2]*times
    return(c(times, x, y))
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


