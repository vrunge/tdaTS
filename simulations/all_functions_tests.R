
library(plotly)

# data: data3D_segment_to_circle
# PD at all time step : Generate_All_Persistence_Diagrams (all_PD)
# plot : Plot_All_Persistence_Diagrams

# remove points (filter all_PD) : remove_noisy_points
# distances (between all_PD) : distances_Persistence_Diagrams

################
#### data segment_to_circle
################


nb_levels <- 20
data <- data3D_segment_to_circle(n = 1000, change = 0.5, time_sampling = "discrete", nb_levels = nb_levels)
data$x <- data$x + rnorm(n= 1000, mean = 0, sd = 0.02)
data$y <- data$y + rnorm(n = 1000, mean = 0, sd = 0.02)
#data <- data3D_segment_to_circle(n = 1000, change = 0.5, time_sampling = "unif")

######## plot the data

minD <- min(data$x, data$y)
maxD <- max(data$x, data$y)
M <- max(minD, maxD)
resScale <- rbind(data,c(0,minD,minD),c(0,minD,maxD),c(0, maxD,minD),c(0,maxD,maxD)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale, x = resScale$x, y = resScale$y, z = resScale$t,
        type="scatter3d", mode = "markers", color = resScale$t)


################
#### all persistent diagrams
################
all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)
dim(all_PD)
all_PD %>% print(n = 100)

################
#### verif with plots and thresholding
################
birth <- Inf
death <- 0
diag <- 0.0001
Plot_All_Persistence_Diagrams(data,
                              birth = birth,
                              death = death,
                              diagonal = diag,
                              nb_levels = nb_levels)


################
#### thresholding
################
all_filter <- remove_noisy_points(all_PD,
                                  birth = birth,
                                  death = death,
                                  diagonal = diag,
                                  infinity = FALSE)


################
#### distances: Wasserstein
################
mydim <- 0 #choice for dimension in PD

t(all_filter[,1:2]) %>% as.matrix()

D_wasserstein_2by2 <- distances_Persistence_Diagrams(all_filter, type = "2by2", dimension = mydim)
par(mfrow = c(1,1))
library(fields)
D_wasserstein_2by2 <- t(D_wasserstein_2by2)
image.plot(D_wasserstein_2by2[,nrow(D_wasserstein_2by2):1], axes = F,  col = grey(seq(0,1, length = 256)))
axis(1, at=seq(0,1,length.out = nb_levels), labels= 1:nb_levels)
axis(2, at=seq(0,1,length.out = nb_levels), labels= nb_levels:1)
segments(0,0.5,1,0.5, col = 2, lwd = 4)
segments(0.5,0,0.5,1, col = 2, lwd = 4)



D_wasserstein_1by1 <- distances_Persistence_Diagrams(all_filter, type = "1by1", dimension = mydim)
par(mfrow = c(1,1))
library(fields)
D_wasserstein_1by1 <- diag(D_wasserstein_1by1)
plot(D_wasserstein_1by1, type = 'b')


###
### TO DO. 05.10.23
###
### adapted Wasserstein truncated Wasserstein truncated
### (using proj_Points function to generating projective points on the boundaries)
### + improve function proj_Points for tibble operations pipe %>%


### Rmd with different tests





