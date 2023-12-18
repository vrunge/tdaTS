

############################################################
### radius density (boundary density)
############################################################

th <- 2*pi*0.1
N <- 1000
alpha <- 0.1
Bound <- 3
r <- seq(0, Bound, length.out = 100)
res <- r^3*(2*pi-th)^3*(1-r^2*(2*pi-th-sin(th))/(2*alpha*N))^(N-3)*r*sin(th/2)
plot(r,res, type = 'b')
res
par(new = TRUE)
res2 <- r^3*(2*pi-th)^3*exp(-r^2*(2*pi-th-sin(th))/(2*alpha))*r*sin(th/2)
plot(r,res2, col = 2, type = 'b')
r[res < 10^(-10)]

th <- 2*pi*0.92
N <- 1000
par(new = TRUE)
res3 <- r^3*(2*pi-th)^3*exp(-r^2*(2*pi-th-sin(th))/(2*alpha))*r*sin(th/2)
res3 <- r^3*(2*pi-th)^3*(1-r^2*(2*pi-th-sin(th))/(2*alpha*N))^(N-3)*r*sin(th/2)
plot(r,res3, col = 3, type = 'b')



the <- seq(0, 2*pi, length.out = 100)
x <- seq(0, 2, length.out = 100)
area <- (2*pi-the + sin(the))/2
areaSquare <- pi*(1- x^2/2)
areaSquare2 <-pi/2*(2- x)^2
MAX <- max(area, areaSquare)
plot(1-cos(1/2*the), area, ylim = c(0,MAX), type = 'l')
par(new = TRUE)
plot(x, areaSquare, ylim = c(0,MAX), type = 'l', col = 3)
par(new = TRUE)
plot(x, areaSquare2, ylim = c(0,MAX), type = 'l', col = 2)
par(new = TRUE)
plot(x, pi/2*(2-x), ylim = c(0,MAX), type = 'l', col = 4)



1-cos(the/2)



x <- seq(0, 1, length.out = 1000)

N <- 10
plot(x,(1-x/N)^(N-3))
par(new = TRUE)
plot(x,exp(-x/N), col =2)
plot(x,(1-x/N)^(N-3) - exp(-x*(N-3)/N))

max(abs((1-x/N)^(N-3) - exp(-x)))
max(abs((1-x/N)^(N-3) - exp(-x*(N-3)/N)))
max(abs((1-x/N)^(N-3) - exp(-x*(N-3)/N))/abs((1-x/N)^(N-3)))

