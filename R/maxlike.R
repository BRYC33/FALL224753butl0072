#' Maximum Likelihood Estimation Function
#'
#' This function calculates the maximum likelihood estimate for a parameter using a specified log likelihood function.
#' @param lfun A log likelihood function.
#' @param x A vector of observed data values.
#' @param param A vector of possible parameter values.
#' @param ... Additional arguments for plot customization.
#' @return A list containing the index, parameter estimate, and log likelihood value at the maximum.
#' @export
#' @examples
#' data <- c(3, 3, 4, 5)
#' mymaxlik(logbin, data, seq(0, 1, length = 1000), xlab = expression(pi), main = "MLE for Binomial")
mymaxlik <- function(lfun, x, param, ...) {
  np <- length(param)
  z <- outer(x, param, lfun)
  y <- apply(z, 2, sum)

  # Plot the log-likelihood function
  plot(param, y, col = "Blue", type = "l", lwd = 2, ...)
  i <- max(which(y == max(y)))
  abline(v = param[i], lwd = 2, col = "Red")
  points(param[i], y[i], pch = 19, cex = 1.5, col = "Black")
  axis(3, param[i], round(param[i], 2))

  return(list(i = i, parami = param[i], yi = y[i]))
}
