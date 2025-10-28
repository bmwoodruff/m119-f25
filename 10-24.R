f <- function(x){(x^2+(8/x)^2)^(1/2)}
curve <- function(x){8/x}

x <- seq(-10,10,0.1)
plot(x, curve(x), type = "l")
plot(x, f(x), type = "l")
