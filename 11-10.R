library(data4led)
bulb <- led_bulb(1,seed=123) #Remember to use your assigned seed!

x <- bulb$hours
y <- bulb$percent_intensity


rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data2_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

x <- c(1,2,3,4,5,6,7)
y <- c(24.1,
       29.5,
       34.5,
       39.1,
       46.6,
       51.9,
       59.1)

c.11 <- sum(2+0*x)
c.12 <- sum(2*x)
c.21 <- sum(2*x)
c.22 <- sum(2*x^2)
b.1 <- sum(2*y)
b.2 <- sum(2*y*x)

best.a2 <- (c.11*b.2 - c.12*b.1)/(c.11*c.22 - c.12^2) 
best.a1 <- (b.1 - c.12*best.a2)/c.11 
best.b <- best.a1
best.m <- best.a2

plot(x,y)
f <- function(x,b=best.b,m=best.m){b+m*x}
xM <- seq(min(x),max(x))
lines(xM,f(xM), type="l")





