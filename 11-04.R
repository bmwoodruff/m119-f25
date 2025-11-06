library(data4led)
bulb <- led_bulb(1,seed=123)
t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t^3)
c.22 <- sum(t^4)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*t^2)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c.11, c.12, b.1, c.12, c.22, b.2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 

best.a1
best.a2

D <- (-c.11)*(-c.22) - (-c.12)^2
D

f2 <- function(x,a0=100,a1=best.a1,a2=best.a2){
  a0 + a1*x + a2*x^2
}

x <- seq(-10,80001,2)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f2(x),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x),col=2)

solveme <- function(x){f2(x)-80}
burnouttime <- uniroot(solveme, c(0,50000))$root
burnouttime

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x),col=2)
abline(h=80,v=burnouttime)









library(data4led)
bulb <- led_bulb(1,seed=123) #Remember to use your assigned seed!

t <- bulb$hours
y <- bulb$percent_intensity

c.11 <- sum(t^2)
c.12 <- sum(t^3)
c.21 <- sum(t^3)
c.22 <- sum(t^4)
b.1 <- sum((y-100)*t)
b.2 <- sum((y-100)*t^2)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c.11, c.12, b.1, c.12, c.22, b.2)
best.a1 <- sol[1] 
best.a2 <- sol[2] 

best.a1
best.a2


fxx <- -sum(t^2)
fxy <- -sum(t^3)
fyy <- -sum(t^4)
D <- fxx*fyy-fxy^2
D
#Since D is positive, we check fxx
fxx
#since fxx is negative, we have a max.


f2 <- function(x,a0=100,a1=best.a1,a2=best.a2){
  a0 + a1*x + a2*x^2
}

x <- seq(-10,80001,2)
par(mfrow=c(1,2),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,f2(x),col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x),col=2)


solveme <- function(x){f2(x)-80}
burnouttime <- uniroot(solveme, c(0,50000))$root
burnouttime

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,f2(x),col=2)
abline(h=80,v=burnouttime)
