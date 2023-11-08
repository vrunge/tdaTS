

#' Plot_All_Persistence_Diagrams
#'
#' @description Function generating a tibble with 3 columns named (t,x,y) with data in the (x,y) plane forming a segment closing to a circle at the change-point location.
#' @param data  tibble with 3 columns (t,x,y) and n rows
#' @param nb_levels number of chunk
#' @param birth vertical threshold
#' @param death horizontal threshold
#' @param diagonal diagonal threshold (distance to the diagonal)
#' @param printProgress print or not the algo progress (TRUE or FALSE)
#' @param barcode plot a barcode (TRUE) or a presistence diagram (FALSE)
#' @return tibble with 3 columns (t,x,y) and n rows
#' @examples
#' data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = 20)
#' Plot_All_Persistence_Diagrams(data, birth = 0.3, death = 0.2, nb_levels = 20)

Plot_All_Persistence_Diagrams <- function(data,
                                          nb_levels = 20,
                                          birth = Inf,
                                          death = 0,
                                          diagonal = 0,
                                          printProgress = FALSE,
                                          barcode = FALSE)
{
  x <- y <- NULL
  opar <- par(no.readonly = TRUE)
  n <- data %>% nrow()

  #############################################################################################
  ###
  ### creating slices of size h such that h * nb_levels = max t - min t
  ###
  w <- data %>% select(t) %>% as.vector
  h <- (max(w$t) - min(w$t))/(nb_levels) * (1 + 10^(-3))
  v <- w$t %/% h
  if(length(unique(data$t)) == nb_levels){v <- 0:(n-1) %/% (n/nb_levels)}
  ###
  ###
  ###

  #############################################################################################
  ###
  ### PLOT 1
  ###
  u <- floor(sqrt(nb_levels))
  if (nb_levels <= u*(u+1)){par(mfrow = c(u+1, u), mar=c(1,2,1,0), mgp=c(1.5,0.5,0))}
  else{par(mfrow = c(u+1, u+1), mar=c(1,2,2,0), mgp=c(1.5,0.5,0))}

  for(k in 0:(nb_levels-1))
  {
    data_slice <- data %>% filter(v == k)

    DiagAlphaCmplx <- alphaComplexDiag(X = data_slice %>% select(-c("t")),
                                       library = c("GUDHI", "Dionysus"),
                                       location = TRUE,
                                       printProgress = printProgress)
      plot(DiagAlphaCmplx$diagram, barcode = barcode, col = 1 + cumsum(DiagAlphaCmplx$diagram[, 1]))

      dd <- DiagAlphaCmplx$diagram[,3]
      dd[is.infinite(dd)] <- NA
      my_max <- max(dd, na.rm = TRUE)
      segments(x0 = birth, y0 = birth + diagonal, x1 = birth, y1 = my_max, col = 2, lwd = 2) #vertical
      segments(x0 = 0, y0 = death, x1 = death - diagonal, y1 = death, col = 2, lwd = 2) #horizontal
      birth2 <- birth
      if(birth2 == Inf){birth2 <- my_max}
      segments(x0 = death - diagonal, y0 = death, x1 = birth2, y1 = birth2 + diagonal, col = 2, lwd = 2) #diagonal
  }
  mtext("Sequence of Persistence Diagrams", side = 3, line = - 2, outer = TRUE)

  #############################################################################################
  ###
  ### PLOT 2
  ###
  if (nb_levels <= u*(u+1)){par(mfrow = c(u+1, u), mar=c(0,0,0,0), mgp=c(1.5,0.5,0))}
  else{par(mfrow = c(u+1, u+1), mar=c(1,2,2,0), mgp=c(1.5,0.5,0))}

  xl <- c(data %>% select(x) %>% min(), data %>% select(x) %>% max())
  yl <- c(data %>% select(y) %>% min(), data %>% select(y) %>% max())

  for(k in 0:(nb_levels-1))
  {
    data_slice <- data %>% filter(v == k)
    X <- data_slice %>% select(-c("t"))
    DiagAlphaCmplx <- alphaComplexDiag(
      X = X,
      library = c("GUDHI", "Dionysus"),
      location = TRUE,
      printProgress = printProgress)
    plot(X, col = 1,xaxt="n", yaxt="n",xlab="", ylab="", xlim = xl, ylim = yl, asp = 1)
    one <- which(DiagAlphaCmplx[["diagram"]][, 1] == 1)
    for (i in seq(along = one))
    {
      for (j in seq_len(dim(DiagAlphaCmplx[["cycleLocation"]][[one[i]]])[1]))
      {
        lines(DiagAlphaCmplx[["cycleLocation"]][[one[i]]][j, , ] + rnorm(n = 4,sd = 0.0), pch = 19, cex = 1, col = i + 1)
      }
    }
  }
  par(opar)
}

