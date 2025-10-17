# Set parameters
A <- 1
b <- .05
k <- 50

# Number of data points
n <- 120

# Generate t values (integers from uniform distribution 1–68)
t <- sample(1:100, n, replace = TRUE)

# Define model function
f <- function(t, A, b, k) {
  A * exp(b * t) + k
}

# Generate residuals from N(0, 10)
residuals <- rnorm(n, mean = 0, sd = 10)

# Compute y values with noise
y <- round(f(t, A, b, k) + residuals,0)

# Combine into a data frame
data <- data.frame(t = t, y = y)

# Show first few rows
head(data)
plot(data$t,data$y)
