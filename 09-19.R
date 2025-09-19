f <- function(x){
  3*x
}

#Did the function definintion work
f(3)

#Define x values, y value, and plot.
x <- -4:7
y <- f(x)
plot(x,y,type = "l")


f <- function(x){
  -2+0*x
}

#Did the function definintion work
f(3)

#Define x values, y value, and plot.
x <- -5:5
y <- f(x)
plot(x,y,type = "l")


#WE can define a single function to do both brain gains.
f <- function(x, m=0, b=1){
  m*x + b
}
f(2)

# First function
x <- -4:7
y <- f(x,m=3,b=0)
plot(x,y,type = "l")

#Second function
x <- -5:5
y <- f(x,m=0,b=-2)
plot(x,y,type = "l")

#Do we have to specify the variable if we want to replace defaults.
x <- -5:5
y <- f(x,7,-2)
plot(x,y,type = "l")

#What about order
#Second function
x <- -5:5
y <- f(x,b=-2,m=0)
plot(x,y,type = "l")
