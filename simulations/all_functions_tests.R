

nb_levels <- 20
data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = nb_levels)
all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)

dim(all_PD)



nb_levels <- 22
data <- segment_to_circle(n = 10000, time_sampling = "unif")

all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)

dim(all_PD)




####

nb_levels <- 20
data <- segment_to_circle(n = 1000, time_sampling = "discrete", nb_levels = nb_levels)
all_PD <- Generate_All_Persistence_Diagrams(data, nb_levels = nb_levels)

v <- 0.4
h <- 0.01
res1 <- all_PD %>%
  filter(Birth < v & Death > h & Death != Inf)
res2 <- remove_noisy_points(all_PD, birth = v, death = h, infinity = FALSE)

res1 %>% print(n = res1 %>% nrow)
res2 %>% print(n = res2 %>% nrow)

remove_noisy_points(all_PD, birth = v, death = h, infinity = FALSE) %>% print(n =100)
remove_noisy_points(all_PD, birth = v, death = h, infinity = TRUE) %>% print(n =100)



