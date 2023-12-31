

#' remove_noisy_points
#'
#' @description Removing points from the persistence diagram out of the vertical, horizontal or diagonal boundaries
#' @param PD Persistence diagram with 4 columns: time, dimension, Birth, Death.
#' @param birth vertical threshold
#' @param death horizontal threshold
#' @param diagonal diagonal threshold (the distance to the diagonal)
#' @param infinity if FALSE we remove points with infinite "Death" values
#' @return The PD filtrated by the 3 boundaries (and the Inf points)
remove_noisy_points <- function(PD, birth = Inf, death = 0, diagonal = 0, infinity = FALSE)
{
  Birth <- Death <- NULL
  if(infinity == TRUE){PD <- PD %>% filter(Birth < birth & Death > death & (Death - Birth > diagonal))}
  if(infinity == FALSE){PD <- PD %>% filter(Birth < birth & Death > death & (Death - Birth > diagonal) & Death != Inf)}
  return(PD)
}



#########################################################################################################

#' proj_Points
#'
#' @description Generating projective points from the persistence diagram in the diagonal, a vertical line and an horizontal line
#' @param PD Persistence diagram with 4 columns:  time, dimension, Birth, Death.
#' @param v vertical threshold
#' @param h horizontal threshold
#' @return Points of the PD projected
proj_Points <- function(PD, v = 0, h = 0)
{
  n <- nrow(PD)
  projPoints <- NULL

  #### points on the horizontal boundary
  if(h > 0)
  {
    for(i in 1:n)
    {
      if((PD[i,3] < h) & (PD[i,4] > h))
        {projPoints <- rbind(projPoints, c(PD[i,1:3], h))} #create
    }
  }

  #### points on the vertical boundary
  if(v > 0)
  {
    for(i in 1:n)
    {
      if((PD[i,3] < v) & (PD[i,4] > v))
      {projPoints <- rbind(projPoints, c(PD[i,1:2], v, PD[i,4]))}
    }
  }

  #### points on the diagonal
  for(i in 1:n)
  {
    m <- mean(unlist(PD[i,3:4]))
    print(m)
  print(h)
  print(v)
    if((m > h) & (m < v)){projPoints <- rbind(projPoints, c(PD[i,1:2], m, m))}
  }

  colnames(projPoints) <- c("time", "dimension", "Birth", "Death")
  return(projPoints)
}

