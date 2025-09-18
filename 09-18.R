(-2)^2
(-1)^2
0^2
1^2
x <- c(-2,-1,0,1,2,3,4,5)
x^2

x <- -2:5
x^2

x <- seq(-2, 5, by = .1)
x
y <- x^2


plot(x,y,type = "l")




#From GPT we got the following
# Define the range of x values
x1 <- seq(-2, 1, length.out = 100)  # x < 1
x2 <- seq(1, 4, length.out = 100)   # x ≥ 1

# Define the function values
y1 <- 2 * x1
y2 <- rep(3, length(x2))

# Plot the first part
plot(x1, y1, type = "l", col = "blue", ylim = c(-4, 6), xlim = c(-2, 4),
     xlab = "x", ylab = "f(x)", main = "Piecewise Function")

# Add the second part
lines(x2, y2, col = "red")

# Add open circle at x = 1 from left side
points(1, 2, pch = 1, col = "blue")  # Open circle at (1,2)

# Add closed circle at x = 1 from right side
points(1, 3, pch = 16, col = "red")  # Filled circle at (1,3)

# Add a legend
legend("topleft", legend = c("2x for x < 1", "3 for x ≥ 1"),
       col = c("blue", "red"), lty = 1, bty = "n")

#Here's our second version

# Define x over a full range
x <- seq(-2, 4, length.out = 500)

# Define the piecewise function using ifelse
f <- ifelse(x < 1, 2 * x, 3)

# Plot the function
plot(x, f, type = "l", lwd = 2, col = "blue",
     xlab = "x", ylab = "f(x)", main = "Piecewise Function")

# Add open circle at (1, 2) -- left-hand limit
points(1, 2, pch = 1, col = "blue")  # Open circle

# Add closed circle at (1, 3) -- right-hand value
points(1, 3, pch = 16, col = "blue")  # Filled circle




# Define the function
f <- function(x) {
  sqrt(x + 4) - sqrt(x + 1) - 1
}

# Solve numerically in a valid interval (x must be ≥ -1)
uniroot(f, interval = c(-1, 100))
