f <- function(x){ x^3 }

x <- seq(-3,3,0.1)
plot(x,f(x), type = "l")
lines(x,f(x+2), type = "l", col=2, xlab = "hi")
lines(x,f(x-2), type = "l", col=3)
lines(x,f(x)+2, type = "l", col=4)
lines(x,f(x)-2, type = "l", col=5)

#We'll generally avoid this option of plotting.
plot(f,-5,5)

rm(list=ls())


f1 <- function(x){
  sqrt(3-x)
}

f1(3)
f1(0)
f1(-100)
f1(10)

x <- seq(-10,3,0.1)
y <- f1(x)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,y,type='l')
?par



f.quad <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quad(-2)
f.quad(-1)
f.quad(0)
f.quad(1)
f.quad(2)

x <- seq(-2,2,0.1)

par(mar=c(2.5,2.5,0.25,0.25))
plot(x,f.quad(x),type='l')


f.quad1 <- function(x,a=1,b=0,c=0){
  a*x^2 + b*x + c
}

f.quad2 <- function(x,a,b,c){
  a*x^2 + b*x + c
}

f.quad1(1/2)
f.quad2(1/2)

f.quad1(1/2)
f.quad2(1/2,1,0,0)

f.quad1(0)
f.quad2(0,1,2,7)

f.quad1(0,1,2,7)
f.quad2(0,1,2,7)

f.quad1(-1/3)

