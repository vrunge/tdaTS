

################################################################################
######## segment_to_circle examples
################################################################################

################################################
######## on uniformly random time
################################################


library(plotly)
my_nb <- 500
res <- segment_to_circle(nb = my_nb, change = 0.5)
resScale <- rbind(res,c(0,-pi,pi),c(0,pi,pi),
                  c(0, pi,-pi),c(0,-pi,-pi)) ### add 4 points in the corners for scaling x and y axes

plot_ly(resScale,
        x=resScale$x,
        y=resScale$y,
        z=resScale$time,
        type="scatter3d",
        mode = "markers",
        color=resScale$time)

################################################
######## on regular time scale
################################################

res <- segment_to_circle(nb = my_nb, change = 0.5, sampling = "discrete", level = 20)
resScale <- rbind(res,c(0,-pi,pi),c(0,pi,pi),
                  c(0, pi,-pi),c(0,-pi,-pi)) ### add 4 points in the corners for scaling x and y axes

plot_ly(resScale,
        x=resScale$x,
        y=resScale$y,
        z=resScale$time,
        type="scatter3d",
        mode = "markers",
        color=resScale$time)




