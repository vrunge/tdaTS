

#' segment_to_circle
#'
#' @description Function generating a data frame with 3 columns (x,y,t) with data in the (x,y) plane forming a segment closing to a circle at the change-point location.
#' @param nb Overall number of points to draw
#' @param change The relative location of the change (example: 0.5 = at the middle of the data)
#' @param sampling unif (uniform continuous time sampling) or discrete (data are located at discrete regular time steps)
#' @param level number of time steps (for option sampling = "discrete")
#' @return A data frame with 3 columns and nb rows
#' @examples
#' segment_to_circle()
segment_to_circle <- function(nb = 1000,
                              change = 0.5,
                              sampling = "unif",
                              level = 20)
{
  #####
  ##### inner function start
  #####
  generator <- function(time, change)
  {
    pos <- runif(1, min = -pi, max = pi)
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

  if(sampling == "unif")
  {
    times <- runif(nb)
    l <- lapply(times, generator, change = change)
  }
  if(sampling == "discrete")
  {
    times <- rep((0:(level-1))/level, each = nb/level)
    l <- lapply(times, generator, change = change)
  }

  res <- data.frame(matrix(unlist(l), nrow = length(l), byrow = TRUE))
  colnames(res) <- c("time","x","y")
  res <- res[order(res$time),]

  return(res)
}

