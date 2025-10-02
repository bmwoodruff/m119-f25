f <- function(x){
  5+3*exp(2*x)-7
}
?uniroot

lower <- -10
upper <- 1
interval <- c(lower,upper)
f(interval)
uniroot(f,interval)$root


f <- function(x){
  (-13+2*x)*exp(-0.05*x)-0
}
interval <- c(-10,10)
f(interval)
uniroot(f,interval)$root

f <- function(x){
  (-13+2*x)*exp(-0.05*x)-5
}
interval <- c(-1,1000)
f(interval)
x <- seq(-10,100,0.1)
plot(x,f(x), type = "l")
# Add horizontal line at y = 0 (Thanks to GPT)
abline(h = 0, col = "red", lty = 2, lwd = 2)

# There are 2 solutions. 
interval <- c(0,20)
uniroot(f,interval)$root

interval <- c(20,100)
uniroot(f,interval)$root





library(data4led)
bulb <- led_bulb(1,seed = 12345)

t <- bulb$hours
y1 <- bulb$percent_intensity

par(mfrow=c(1,1),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour", ylab="Intensity(%) ", pch=16)

f2 <- function(x,a0=0,a1=0,a2=1){ a0 + a1*x + a2*x^2 }

x <- seq(-10,80001,2)
yM <- f2(x, 
         a0=100,
         a1=0.0017,
         a2=-0.00000026
         )

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f2')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)



f5 <- function(x,a0=100,a1=0,a2=1){ (a0 + a1*x)*exp(-a2*x) }

x <- seq(-10,800001,2)
yM <- f5(x,
         a0=100,
         a1=0.00487,
         a2=0.0000425
         )

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f5')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)



f1 <- function(x,a0=0,a1=0){ a0 + a1*x }

x <- seq(-10,80001,2)
yM <- f1(x,a0=100,a1=0.00067)

par(mfrow=c(1,2),mar=c(2,2,3,0.25),oma=rep(0.5,4))
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16,main='f1')
lines(x,yM,col=2)
plot(t,y1,xlab="Hour ", ylab="Intensity(%) ", pch=16, xlim = c(-10,80000),ylim = c(-10,120))
lines(x,yM,col=2)
