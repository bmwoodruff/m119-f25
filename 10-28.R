f <- function(x){6/4*x-1/4*x^3}

low <- 0
high <- 3
x <- seq(low,high, 0.1)

plot(x,f(x), type="l", col="red")

dfdx <- function(x){6/4*1-1/4*3*x^2}
lines(x,dfdx(x), col="blue")
abline(h=0)
(root <- uniroot(dfdx,c(1,2))$root)

abline(v=root, col = "green")
