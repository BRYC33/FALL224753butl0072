# Inform R that 'x' is used globally in the curve() function
utils::globalVariables("x")

# R/mycltp.R
#' Central Limit Theorem for Poisson Distribution
#'
#' This function generates a sample from a Poisson distribution, calculates sample means,
#' and plots the histogram of the sample means along with a theoretical normal curve.
#'
#' @param n Sample size for each iteration
#' @param iter Number of iterations
#' @param lambda Poisson parameter
#' @param ... Additional graphical parameters
#'
#' @return A histogram of sample means and a theoretical normal curve
#' @export
mycltp <- function(n, iter, lambda = 4, ...) {
  # Generate random samples from a Poisson distribution
  y <- rpois(n * iter, lambda = lambda)

  # Reshape the data into a matrix (n rows, iter columns)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  # Apply the mean function to each column (2 indicates columns)
  w <- apply(data, 2, mean)

  # Create the histogram of the sample means
  param <- hist(w, plot = FALSE)
  ymax <- 1.1 * max(param$density)  # Calculate ymax for density plot scaling

  # Plot the histogram with density
  hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(length(param$mids)),
       main = paste("Histogram of Sample Mean\nSample Size = ", n, " Iter = ", iter, " Lambda = ", lambda),
       xlab = "Sample Mean", ...)

  # Add the theoretical normal curve (Central Limit Theorem)
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda / n)), add = TRUE, col = "Red", lty = 2, lwd = 3)
}
