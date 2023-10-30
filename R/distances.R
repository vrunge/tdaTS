

#' distances_Persistence_Diagrams
#'
#' @description Function computing distances between Persistence Diagrams
#' @param PD Persistence diagram with 4 columns:  time, dimension, Birth, Death.
#' @param distance Type of distance used
#' @param type 1by1 (segment versus segment) comparisons or 2by2 (PD by PD) comparisons
#' @param dimension dimension to chose in Persitence diagram
#' @examples
#' data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = 20)
#' all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = 20)
#' distances_Persistence_Diagrams(all_PD)
distances_Persistence_Diagrams <- function(PD, distance = "wasserstein", type = "2by2", dimension = 1)
{
  time <- NULL
  nb_levels <- max(PD$time)

  if(type == "2by2")
  {
    D_wasserstein <- matrix(0, nrow = nb_levels + 1, ncol = nb_levels + 1)
    for(i in 0:(nb_levels))
    {
      for(j in 0:(nb_levels))
      {
        u <- as.matrix(PD %>% filter(time == i) %>% select(-"time"))
        v <- as.matrix(PD %>% filter(time == j) %>% select(-"time"))
        D_wasserstein[i+1,j+1] <- TDA::wasserstein(u, v, p = 2, dimension = dimension)
      }
    }
  }
  if(type == "1by1")
  {
    D_wasserstein <- matrix(0, nrow = nb_levels, ncol = nb_levels)
    for(i in 0:(nb_levels-1))
    {
      u <- as.matrix(PD %>% filter(time <= i) %>% select(-"time"))
      v <- as.matrix(PD %>% filter(time > i) %>% select(-"time"))
      D_wasserstein[i+1,i+1] <- TDA::wasserstein(u, v, p = 2, dimension = dimension)
    }
  }

  return(D_wasserstein)
}
