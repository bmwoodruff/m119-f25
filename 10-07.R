rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y

#this is for finding when dS/da=0
b1 <- -2*sum(y*exp(-x))
c1 <- 2*sum(exp(-x)^2)
(best_a <- -b1/c1)

f <- function(x, a=best_a){a*exp(-x)}
t <- seq(-2,4,0.1)


plot(x,y)
lines(t, f(t))


#Let's check if the second derivative is positive.
c1




rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data1_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
data <- read.csv(url("https://byuistats.github.io/M119/data2_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
data <- read.csv(url("https://byuistats.github.io/M119/data3_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
data <- read.csv(url("https://byuistats.github.io/M119/data4_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)
data <- read.csv(url("https://byuistats.github.io/M119/data5_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)



rm(list=ls())
data <- read.csv(url("https://byuistats.github.io/M119/data1_ls.csv"))
x <- data$x
y <- data$y
plot(x,y)

#this is for finding when dS/dm=0
b1 <- -2*sum(x*y)
c1 <- 2*sum(x^2)
(best_m <- -b1/c1)

f <- function(x, m=best_m){m*x}
t <- seq(-5,5,0.1)

plot(x,y)
lines(t, f(t))

