

################
#### data
################

nb_levels <- 20
data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = nb_levels)
#data <- segment_to_circle(n = 1000, time_sampling = "unif")

################
#### all persistent diagrams
################
all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)
dim(all_PD)


################
#### verif with plots and thresholding
################
birth <- 0.3
death <- 0.2
Plot_All_Persistence_Diagrams(data, birth = birth, death = death, nb_levels = nb_levels)


################
#### thresholding
################
remove_noisy_points(all_PD, birth = birth, death = death, infinity = FALSE) %>% print(n =100)

################
#### distances: Wasserstein truncated
################




