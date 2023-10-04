


#' Generate_All_Persistence_diagrams
#'
#' @description
#' @param data
#' @param nbLevel
#' @return Points of the PD projected
Generate_All_Persistence_diagrams <- function(data, nbLevel = 20)
{
  n <- 400
  All_PD <- NULL
  for(k in 0:(level-1))
  {
    tau <- n*k/level
    tau2 <- n*(k+1)/level
    res_slice <- data[(tau+1):tau2,]
    X <- res_slice[,-1]
    DiagAlphaCmplx <- alphaComplexDiag(
      X = X,
      library = c("GUDHI", "Dionysus"),
      location = TRUE,
      printProgress = TRUE)

    diagramAndNB <- cbind(k, DiagAlphaCmplx$diagram)
    All_PD <- rbind(All_PD, diagramAndNB)
  }
  colnames(All_PD)[1] <- "time"
  All_PD <- as.data.frame(All_PD)

  return(ALL_PD)
}



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



#' remove_noisy_points
#'
#' @description Removing points from the persistence diagram out of the vertical and horizontal boundaries
#' @param PD Persistence diagram with 4 columns:  time, dimension, Birth, Death.
#' @param v vertical threshold
#' @param h horizontal threshold
#' @return The PD filtrated by the 2 boundaries
remove_noisy_points <- function(PD, v = 0, h = 0, infinity = FALSE)
{
  n <- nrow(PD)

  #### points on the horizontal boundary
  if(h > 0)
  {
    for(i in 1:n)
    {
      if(PD[i,4] <= h){PD[i,1] <- -1} #remove
    }
  }
  #### points on the vertical boundary
  if(v > 0)
  {
    for(i in 1:n)
    {
      if(PD[i,3] >= v){PD[i,1] <- -1}
    }
  }
  if(infinity == FALSE)
  {
    for(i in 1:n)
    {
      if(PD[i,4] == Inf){PD[i,1] <- -1}
    }
  }

  PD <- PD[PD$time >= 0,]
  return(PD)
}

