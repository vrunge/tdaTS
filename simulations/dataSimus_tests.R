
library(plotly)

################################################################################################
################
#### data circle_move_distortion
################

nb_levels <- 20
data <- circle_move_distortion(n = 10000, rotation = 2, X_rate = 0.5, Y_rate = 3)

######## plot the data

minD <- min(data$x, data$y)
maxD <- max(data$x, data$y)
M <- max(minD, maxD)
resScale <- rbind(data,c(0,minD,minD),c(0,minD,maxD),c(0, maxD,minD),c(0,maxD,maxD)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale, x = resScale$x, y = resScale$y, z = resScale$t,
        type="scatter3d", mode = "markers", color = resScale$t)

################################################################################################
################
#### data segment_to_segments
################

data <- segment_to_segments(X_rate = 2, gap = 0.1)


minD <- min(data$x, data$y)
maxD <- max(data$x, data$y)
M <- max(minD, maxD)
resScale <- rbind(data,c(0,minD,minD),c(0,minD,maxD),c(0, maxD,minD),c(0,maxD,maxD)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale, x = resScale$x, y = resScale$y, z = resScale$t,
        type="scatter3d", mode = "markers", color = resScale$t)

################################################################################################
################
#### data circle_extinction
################

data <- circle_extinction(change = 0.9)

minD <- min(data$x, data$y)
maxD <- max(data$x, data$y)
M <- max(minD, maxD)
resScale <- rbind(data,c(0,minD,minD),c(0,minD,maxD),c(0, maxD,minD),c(0,maxD,maxD)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale, x = resScale$x, y = resScale$y, z = resScale$t,
        type="scatter3d", mode = "markers", color = resScale$t)


################################################################################################
################
#### data segment_to_circle
################

nb_levels <- 20
data <- segment_to_circle(n = 1000, change = 0.5, time_sampling = "discrete", nb_levels = nb_levels)
data$x <- data$x + rnorm(n= 1000, mean = 0, sd = 0.2)
data$y <- data$y + rnorm(n = 1000, mean = 0, sd = 0.2)
data <- segment_to_circle(n = 1000, change = 0.5, time_sampling = "unif")

######## plot the data

minD <- min(data$x, data$y)
maxD <- max(data$x, data$y)
M <- max(minD, maxD)
resScale <- rbind(data,c(0,minD,minD),c(0,minD,maxD),c(0, maxD,minD),c(0,maxD,maxD)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale, x = resScale$x, y = resScale$y, z = resScale$t,
        type="scatter3d", mode = "markers", color = resScale$t)

