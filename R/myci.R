#' Calculate a 95% Confidence Interval for the Mean of a Single Sample
#'
#' Computes a 95% confidence interval for the mean of a single numeric sample using the t-distribution.
#'
#' @param x A numeric vector representing the sample data.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the 95% confidence interval.
#'
#' @details This function calculates the sample mean and standard error, and uses the t-distribution
#' to compute the 95% confidence interval. It assumes the sample is drawn from a normally distributed
#' population or that the sample size is large enough for the Central Limit Theorem to apply.
#'
#' @examples
#' # Example usage with a random sample
#' set.seed(23)
#' x <- rnorm(30, mean = 10, sd = 12)
#' myci(x)
#'
#' @export
myci <- function(x) {
  # Calculate sample mean and standard deviation
  mean_x <- mean(x)
  sd_x <- sd(x)
  n <- length(x)

  # Calculate the standard error
  se <- sd_x / sqrt(n)

  # Calculate the critical value for 95% confidence
  t_value <- qt(0.975, df = n - 1)  # Two-tailed 95% CI

  # Calculate the confidence interval
  lower_bound <- mean_x - t_value * se
  upper_bound <- mean_x + t_value * se

  # Return the confidence interval as a vector
  c(lower_bound, upper_bound)
}
