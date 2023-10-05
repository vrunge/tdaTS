

################
#### data
################

nb_levels <- 20
data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = nb_levels)
#data <- segment_to_circle(n = 1000, time_sampling = "unif")

######## plot the data
library(plotly)
resScale <- rbind(data,c(0,-pi,pi),c(0,pi,pi),c(0, pi,-pi),c(0,-pi,-pi)) ### add 4 points in the corners for scaling x and y axes
plot_ly(resScale, x = resScale$x, y = resScale$y, z = resScale$t,
        type="scatter3d", mode = "markers", color = resScale$t)


################
#### all persistent diagrams
################
all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)
dim(all_PD)
all_PD %>% print(n = 1000)

################
#### verif with plots and thresholding
################
birth <- 0.3
death <- 0.02
Plot_All_Persistence_Diagrams(data, birth = birth, death = death, nb_levels = nb_levels)


################
#### thresholding
################
#all_PD <- remove_noisy_points(all_PD, birth = birth, death = death, infinity = FALSE)


################
#### distances: Wasserstein
################

D_wasserstein <- distances_Persistence_Diagrams(all_PD, type = "2by2")

library(fields)
D_wasserstein <- t(D_wasserstein)
image.plot(D_wasserstein[,nrow(D_wasserstein):1], axes = F,  col = grey(seq(0,1, length = 256)))
axis(1, at=seq(0,1,length.out = nb_levels), labels= 1:nb_levels)
axis(2, at=seq(0,1,length.out = nb_levels), labels= nb_levels:1)
segments(0,0.5,1,0.5, col = 2, lwd = 4)
segments(0.5,0,0.5,1, col = 2, lwd = 4)


###
### TO DO. 05.10.23
###
### adapted Wasserstein truncated Wasserstein truncated
### (using proj_Points function to generating projective points on the boundaries)
### + improve function proj_Points for tibble operations pipe %>%


