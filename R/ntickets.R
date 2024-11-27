#' Calculate Optimal Number of Tickets to Sell
#'
#' This function calculates the optimal number of tickets to sell based on
#' a binomial distribution (discrete case) and its normal approximation (continuous case).
#'
#' @param N Total number of tickets available
#' @param gamma The risk level (probability of exceeding the available seats)
#' @param p The probability of a ticket holder showing up
#'
#' @return A list containing the calculated number of tickets for the discrete case (`nd`)
#' and for the continuous case (`nc`), as well as the input parameters.
#' @export
#' @importFrom stats pbinom qbinom qnorm
#' @importFrom graphics abline
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N = 200, gamma = 0.02, p = 0.95) {
  # Generate a sequence of potential ticket sales values
  n <- seq(N, floor(N + N/10), by = 1)

  # Calculate the temporary values using the binomial distribution (discrete case)
  tmp_discrete <- 1 - gamma - pbinom(q = N, size = n, prob = p)

  # Find the index of the minimum absolute difference (for optimal value) for discrete case
  ind_discrete <- which.min(abs(tmp_discrete))

  # Calculate the normal approximation for the continuous case
  mu <- N * p
  sigma <- sqrt(N * p * (1 - p))
  tmp_continuous <- 1 - gamma - pnorm(n, mean = mu, sd = sigma)

  # Find the index of the minimum absolute difference (for optimal value) for continuous case
  ind_continuous <- which.min(abs(tmp_continuous))

  if (interactive()) {
    # Plot for discrete case
    plot(n, tmp_discrete, type = "b", col = "blue", lwd = 1.5,
         main = paste("Objective vs n to Find Optimal Tickets Sold\n", "Discrete"),
         xlab = "n", ylab = "Objective (Discrete)")
    abline(h = 0, col = "red")
    abline(v = n[ind_discrete], col = "blue", lwd = 1.5)

    # Plot for continuous case (on a new plot)
    plot(n, tmp_continuous, type = "b", col = "green", lwd = 1.5,
         main = paste("Objective vs n to Find Optimal Tickets Sold\n", "Continuous"),
         xlab = "n", ylab = "Objective (Continuous)")
    abline(h = 0, col = "red")
    abline(v = n[ind_continuous], col = "green", lwd = 1.5)
  }

  # Return the results as a list
  return(list(nd = n[ind_discrete], nc = n[ind_continuous], N = N, p = p, gamma = gamma))
}

# Example call to the function
ntickets(N = 400, gamma = 0.25, p = 0.95)



