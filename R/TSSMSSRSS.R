
#' Title Calculate TSS, MSS, RSS
#'
#' @param actual A numeric vector representing the actual observed values (e.g., the Height of trees)
#' @param predicted A numeric vector representing the predicted values (e.g., from a linear model)
#'
#' @return A list containing TSS, MSS, and RSS
#' @export
#'
#' @examples
#' actual <- c(10, 15, 20, 25, 30)
#' predicted <- c(12, 16, 19, 24, 28)
#' calculate_sums_of_squares(actual, predicted)
calculate_sums_of_squares <- function(actual, predicted) {
  # Mean of the actual values
  mean_actual <- mean(actual)

  # Total Sum of Squares (TSS)
  TSS <- sum((actual - mean_actual)^2)

  # Model Sum of Squares (MSS)
  MSS <- sum((predicted - mean_actual)^2)

  # Residual Sum of Squares (RSS)
  RSS <- sum((actual - predicted)^2)

  # Return a list with the results
  return(list(TSS = TSS, MSS = MSS, RSS = RSS))
}


