

n = 1000
nb_levels = 11
v <- 0:(n-1) %/% (n/nb_levels)
length(v)
table(v)

v <- 0:(n-1) %/% (n/nb_levels)
v/(nb_levels-1)
