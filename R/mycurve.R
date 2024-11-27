#' @title mycurve
#' @param mu Numeric, the mean of the normal distribution.
#' @param sigma Numeric, the standard deviation of the normal distribution.
#' @param a Numeric, the value up to which the area under the curve is shaded.
#'
#' @return A list containing the mean (`mu`), the standard deviation (`sigma`),
#' and the calculated cumulative probability (`probability`).
#' @export
#'
#' @examples
#' mycurve(mu = 10, sigma = 5, a = 6)
mycurve <- function(mu, sigma, a) {
  # Define the x range for the plot
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 1000)

  # Create the normal curve
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3 * sigma, mu + 3 * sigma),
        main = paste("Normal Curve with mu =", mu, "and sigma =", sigma))

  # Shade the region where x <= a
  x_fill <- seq(mu - 3 * sigma, a, length = 1000)
  y_fill <- dnorm(x_fill, mean = mu, sd = sigma)
  polygon(c(x_fill, a, mu - 3 * sigma), c(y_fill, 0, 0), col = "skyblue")

  # Calculate the probability P(X <= a)
  prob <- pnorm(a, mean = mu, sd = sigma)

  # Return the parameters and the calculated probability
  list(mu = mu, sigma = sigma, probability = prob)
}
