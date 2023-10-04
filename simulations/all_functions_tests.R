

nb_levels <- 20
data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = nb_levels)

all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)

dim(all_PD)



nb_levels <- 22
data <- segment_to_circle(n = 10000, time_sampling = "unif")

all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)

dim(all_PD)
