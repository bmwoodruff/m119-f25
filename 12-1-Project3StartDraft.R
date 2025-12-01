library(data4soils)
Ng <- cfbp_fpjuliet$ng

mean(Ng)
var(Ng)

mu = mean(Ng)
sigma = sqrt(var(Ng))

hist(Ng)


f1 <- function(x,m=0,s=1){1/(sqrt(2*pi*s^2))*exp(-0.5*((x-m)/s)^2)}

mu <- mean(Ng)
sigma <- sqrt(var(Ng))
#sigma <- sd(Ng)

x1 <- seq(-20,20,0.1)
y1 <- f1(x1,m=mu,s=sigma)

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Normal",xlim=c(-5,12))
lines(x1,y1,col=2)


f2 <- function(x,a=1,b=1){b^a/gamma(a)*(x)^(a-1)*exp(-b*x)}

alpha <- mean(Ng)^2/var(Ng)
alpha
beta <- mean(Ng)/var(Ng)
beta

x2 <- seq(0,20,0.1)
y2 <- f2(x2,a=alpha,b=beta)

par(mfrow=c(1,1),mar=c(2.5,2.5,1,0.25))
hist(Ng, probability = TRUE, main="Fitted Gamma")
lines(x2,y2,col=2)
