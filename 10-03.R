f <- function(t){ 
  log(3*t, base=2) - 2
}

f(2)

fequals2 <- function(t){
  f(t) - 2
} 

interval <- c(1,10)
uniroot(fequals2, interval)$root


f <- function(t, rhs = 0){ 
  log(3*t, base=2) - 2 - rhs
}
interval <- c(1,10)
uniroot(f, interval, rhs=0)$root
#you can pass extra arguments into your function, 
#at the end of uniroot.


#Another option is anonymous function notation. (lambda notation in python)
f <- function(t){ 
  log(3*t, base=2) - 2
}
uniroot(function(t) {f(t) - 2}, c(1,10))$root



#Group Practice
g <- function(x){
  3*x-15-exp(-x+6)
}
uniroot(g,c(0,10))$root
x <-seq(0,30,1)
plot(x,g(x), type="l")
abline(h=0, col = "lightgray")


f <- function(x){
  3*x-5
}
uniroot(f,c(0,10))$root
x <-seq(0,30,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")


f <- function(x){
  3*x-5 - 7
}
uniroot(f,c(0,10))$root
x <-seq(0,30,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")


f <- function(x){
  3*x-5 - exp(-x)
}
uniroot(f,c(0,10))$root
x <-seq(0,30,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")

f <- function(x){
  3*x-5 - log(x)
}
uniroot(f,c(1,2))$root
x <-seq(0,5,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")


f <- function(x){
  x^2+x-6
}
uniroot(f,c(1,10))$root
x <-seq(-5,5,1)
plot(x,f(x), type="l")
abline(h=0, col = "lightgray")
uniroot(f,c(-5,0))$root
uniroot(f,c(0,5))$root





rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=0,a2=1){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=0,a1=0,a2=1){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f2(x,a0=100,a1=0,a2=0)
y1 <- f2(x,a0=100,a1=7e-4,a2=0)
y2 <- f2(x,a0=100,a1=1.1e-3,a2=-1.5e-7)
y3 <- f3(x,a1=-1.9,a2=0.00114)
y4 <- f4(x,a0=100,a1=-1.81e-4,a2=0.83)
y5 <- f5(x,a0=100,a1=6.23e-3,a2=5.06e-5)


par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f0')
lines(x,y0,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y0,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,y1,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y1,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,y2,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y2,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f3')
lines(x,y3,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y3,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f4')
lines(x,y4,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y4,col=2)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,y5,col=2)
plot(t,y,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,y5,col=2)


f2is80 <- function(t){
  f2(t,a0=100,a1=1.1e-3,a2=-1.5e-7) - 80
}


uniroot(f2is80, c(0,20000))$root




