# Define the piecewise function f
f <- function(x) {
  if (x < -1) {
    return(x^3)
  } else if (x > -1 && x < 4) {
    return(-2)
  } else if (x >= 4) {
    # For x >= 4
    return(sqrt(x))
  } else {
    # This handles x == -1 exactly (which is undefined in the problem)
    return(NA)
  }
}

# Values to evaluate
xs <- c(-2, -1, 4)

# Compute f at those points
results <- sapply(xs, f)

# Combine into a data frame for nice display
data.frame(
  x = xs,
  f_x = results
)
