% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PersistenceDiagrams_plots.R
\name{Plot_All_Persistence_Diagrams}
\alias{Plot_All_Persistence_Diagrams}
\title{Plot_All_Persistence_Diagrams}
\usage{
Plot_All_Persistence_Diagrams(
  data,
  nb_levels = 20,
  birth = Inf,
  death = 0,
  diagonal = 0,
  printProgress = FALSE,
  barcode = FALSE
)
}
\arguments{
\item{data}{tibble with 3 columns (t,x,y) and n rows}

\item{nb_levels}{number of chunk}

\item{birth}{vertical threshold}

\item{death}{horizontal threshold}

\item{diagonal}{diagonal threshold (distance to the diagonal)}

\item{printProgress}{print or not the algo progress (TRUE or FALSE)}

\item{barcode}{plot a barcode (TRUE) or a presistence diagram (FALSE)}
}
\value{
tibble with 3 columns (t,x,y) and n rows
}
\description{
Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a segment closing to a circle at the change-point location.
}
\examples{
data <- data3D_segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = 20)
Plot_All_Persistence_Diagrams(data, birth = 0.3, death = 0.2, nb_levels = 20)
}
