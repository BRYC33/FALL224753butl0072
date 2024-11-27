#' Title: Simulate Binomial Trials
#'
#' @param n_trials - integer, number of trials in each experiment
#' @param prob_success - double, probability of success in each trial
#' @param n_experiments - integer, number of experiments to run
#'
#' @return A data frame with the results of each experiment
#' @export
#'
#' @examples
#' simulate_binomial() 
#' simulate_binomial(10, 0.5, 100)
simulate_binomial <- function(n_trials = 10, prob_success = .5, n_experiments = 100) {
  results <- rbinom(n_experiments, n_trials, prob_success)
  return(data.frame(Experiment = 1:n_experiments, Successes = results))
}

simulate_binomial() 