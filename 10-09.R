rm(list=ls())

###Define the distribution###
p <- function(x,lambda=1){
  # x must be a whole number
  (lambda^x/factorial(x))*exp(-lambda)
}

###Define the likelihood function###
LP <- function(lambda,x){
  # The element of x must be a whole numbers.
  prod(p(x,lambda))
}

###Possible Parameter Values###
lambda <- seq(0,10,0.001)

###Data###
# Florida Hurricane Data (2000-2022)
data <- c(4,4,8,8,6,8,2,8,8,4,8,6,4,3,2,4,6,7,4,7,13,3,3)

#Here we calculate the output from the likelihood function given the observed data.
y <- sapply(lambda,FUN=LP,x=data)

#We plot the likelihood function.
par(mar=c(2.5,2.5,3,0.25))
plot(lambda,y,type='l',main='Poisson Likelihood')




p.3v1 <- function(x,lambda=2){
  # each element of x must be a whole number
  prod((lambda^x/factorial(x))*exp(-lambda))
}

p.3v1(c(8,2,8,8,4),lambda = 2)
p.3v1(c(8,2,8,8,4),lambda = 5.5)
lambdas <- seq(5,7,0.1)
likelihoods <- sapply(lambdas, p.3v1,x = c(8,2,8,8,4))
plot(lambdas,likelihoods)

lambda <- seq(0,15,0.01)
x <- c(8,2,8)
p <- sapply(lambda, p.3v1, x = x)
plot(lambda,p,type='l')
