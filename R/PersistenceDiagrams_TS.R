
#' Generate_All_Persistence_Diagrams
#'
#' @description Function generating a sequence of Persistence Diagrams using tibble data with columns (time,x,y)
#' @param data tibble data with columns (time,x,y)
#' @param nb_levels number of regular slices to be considered for the time variable
#' @return tibble of the sequence of Persistence Diagrams with columns (time, dimension, Birth, Death). "time" is the position of the persistence diagram in the sequence of diagrams.
Generate_All_Persistence_Diagrams <- function(data, nb_levels = 20)
{
  n <- data %>% nrow()

  ###
  ### creating slices of size h such that h * nb_levels = max t - min t
  ###
  w <- data %>% select(t) %>% as.vector
  h <- (max(w$t) - min(w$t))/(nb_levels) * (1 + 10^(-3))
  v <- w$t %/% h
  if(length(unique(data$t)) == nb_levels){v <- 0:(n-1) %/% (n/nb_levels)}
  ###
  ###

  All_PD <-  matrix(ncol = 4, nrow = 0)
  for(k in 0:(nb_levels-1))
  {
    data_slice <- data %>% filter(v == k)

    DiagAlphaCmplx <- alphaComplexDiag(
                          X = data_slice %>% select(-c("t")),
                          library = c("GUDHI", "Dionysus"),
                          location = TRUE,
                          printProgress = TRUE)

    diagramAndNB <- cbind(k, DiagAlphaCmplx$diagram)
    All_PD <- rbind(All_PD, diagramAndNB)
  }
  colnames(All_PD)[1] <- "time"
  All_PD <- as_tibble(All_PD)

  return(All_PD)
}


