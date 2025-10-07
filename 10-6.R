#Ammon's Question.
#What does OUT do?   It's just a variable name.  This method of code
#initializes a vector of 0s, before replacing them.
f4 <- function(x,lambda=1){
  
  out <- rep(0,length(x))
  out[(x > 0)] <- 1 - exp(-lambda*x[(x > 0)])
  
  return(out)
}
#An ifelse statement works the same, as ifelse is properly "vectorized".
f4second <- function(x,lambda=1){
  ifelse(x>0, 1 - exp(-lambda*x), 0)
}
x <- seq(-10,10)
f4(x)
f4second(x)

#Let's revisit Friday's material, this time setting the
#default values in the definition of the function.

rm(list=ls())
library(data4led)
bulb <- led_bulb(1,seed = 123)

t <- bulb$hours
y <- bulb$percent_intensity

f0 <- function(x,a0=100 + 0*x ){ a0 }
f1 <- function(x,a0=100,a1=7e-4){ a0 + a1*x }
f2 <- function(x,a0=100,a1=1.1e-3,a2=-1.5e-7){ a0 + a1*x + a2*x^2 }
f3 <- function(x,a1=-1.9,a2=0.00114){ (100-a1) + a1*exp(-a2*x) }
f4 <- function(x,a0=100,a1=-1.81e-4,a2=0.83){a0+a1*x+a2*log(0.005*x+1)}
f5 <- function(x,a0=100,a1=6.23e-3,a2=5.06e-5){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,80001,2)
y0 <- f0(x)
y1 <- f1(x)
y2 <- f2(x)
y3 <- f3(x)
y4 <- f4(x)
y5 <- f5(x)


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

#Use the model f4 to answer the question, What is the intensity of the bulb after 12000 hours?
f4(12000)
#Use the model f3 to answer the question, When is the intensity of the bulb 90% of its original intensity?
f3is90 <- function(x){
  f3(x)-90
}
uniroot(f3is90,c(-10000,0))$root
#uniroot gives a negative value, which makes no sense in the context here.
uniroot(function(x) {f3(x)-90} ,c(-10000,0))$root


#f2 When is the intensity of the bulb 97% of its original intensity?
uniroot(function(x) {f2(x)-97} ,c(0,20000))$root

#f5 When is the intensity of the bulb 95% of its original intensity?
uniroot(function(x) {f5(x)-95} ,c(0,20000))$root
#What is the intensity of the bulb after 25000 hours?
f5(25000)


p <- function(x,lambda=2){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

p(1,lambda=1)
p(0,lambda=1)
p(2,lambda=1)
p(3,lambda=1)
#P(Y>1)
1 - ( p(0,lambda=1) + p(1,lambda=1) )
inputs <- seq(2,100)
sum(p(inputs, lambda=1))

inputs <- seq(0,20)
inputs
p(inputs, lambda=1)
#The probability that 0<=Y<=20
sum(p(inputs, lambda=1))


# Group Activity
sum(p(0:7, lambda = 3))


lambdas <- seq(6,8,0.1)
plot(lambdas, p(7, lambdas))



## Three year stuff
p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v2 <- function(x1,x2,x3,lambda=2){
  # x1, x2, and x3 must be whole numbers
  (lambda^(x1+x2+x3)/(factorial(x1)*factorial(x2)*factorial(x3)))*exp(-3*lambda)
}


#The probability of 4 Florida tropical storms this year, 4 Florida tropical storms next year, and 8 Florida tropical storms the year after (using $\lambda = 2$ as assumed).
p.3v1(c(4,4,8))
#The same probability as above, using the other version
p.3v2(4,4,8)
#The same probabilty as above by just multiplying probabilities of independent events together. 
p(4)*p(4)*p(8)

#The probability of 2 Florida tropical storms this year, 5 Florida tropical storms next year, and 3 Florida tropical storms the year after.
p.3v1(c(2,5,3))

