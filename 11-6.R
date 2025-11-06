data <- read.csv(url("https://byuistats.github.io/M119/logLikelihood_practice.csv"))

x <- data$x
y <- data$y2

head(data)
plot(x,y)


c11 <- sum(x^2)
c12 <- sum(x)
c21 <- sum(x)
c22 <- 50
b1 <- sum(y*x)
b2 <- sum(y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c12, c22, b2)
sol
best_m <- sol[1] 
best_b <- sol[2] 

best_m
best_b




c11 <- 50
c12 <- sum(x)
c21 <- c12
c22 <- sum(x^2)
b1 <- sum(y)
b2 <- sum(x*y)

## Create a function to solve a system of equations. 
solvesystem <- function(c11, c12,b1,c21,c22,b2){ 
  c((b1*c22 - c12*b2)/(c11*c22 - c21*c12),
    (c11*b2 - b1*c21)/(c11*c22 - c21*c12))
}

sol <- solvesystem(c11, c12, b1, c12, c22, b2)
best_b <- sol[1] 
best_m <- sol[2] 

best_b
best_m
sol
