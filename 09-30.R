f <- function(x){3^x-17}
interval <- c(2,3)
uniroot(f, interval)
log(17,3)
my_root <- uniroot(f, interval)$root

#The code below produces a plot that illustrates what uniroot found. 
x<-seq(interval[1],interval[2],0.1)
plot(x,f(x),type="l")
abline(h=0,col = "gray")
abline(v=my_root,col = "gray")
points(my_root,0, col = "red")


f <- function(x){log(4*x-2)-5}
x <- seq(1,100)
plot(x,f(x))
interval <- c(1,100)
f(interval)
uniroot(f,interval)$root

f <- function(x){x+exp(x)-3}
x <- seq(-2,2,0.1)
plot(x,f(x),type="l")
interval <- c(-2,2)
f(interval)
uniroot(f,interval)$root
