#' Calculate and Plot p-Value for t-Statistic
#'
#' This function calculates the p-value for a given t-statistic and plots the
#' area under the t-distribution curve corresponding to the p-value.
#'
#' @param t0 The calculated t-statistic.
#' @param xmax The maximum x-axis limit for the plot. Default is 4.
#' @param n The sample size. Default is 20.
#' @param alpha The significance level for the plot. Default is 0.05.
#' @return A list containing the critical value (q) and the p-value.
#' @examples
#' # Calculate and plot p-value for t0 = 2 with sample size n = 30
#' Yourpackage::mypvalue(t0 = 2, n = 30)
#'
#' @export
mypvalue <- function(t0, xmax = 4, n = 20, alpha = 0.05) {
  va <- round(pt(-t0, df = n - 1), 4)
  pv <- 2 * va

  # Plot the t-distribution
  curve(dt(x, df = n - 1), xlim = c(-xmax, xmax), ylab = "T Density", xlab = expression(t),
        main = substitute(paste("P-value=", pv, " alpha=", alpha)))

  # Set up points for the right polygon
  xcurve <- seq(t0, xmax, length = 1000)
  ycurve <- dt(xcurve, df = n - 1)

  # Set up points for the left polygon
  xlcurve <- seq(-t0, -xmax, length = 1000)
  ylcurve <- dt(xcurve, df = n - 1)

  # Shade in polygons
  polygon(c(t0, xcurve, xmax), c(0, ycurve, 0), col = "green")
  polygon(c(-t0, xlcurve, -xmax), c(0, ylcurve, 0), col = "green")

  # Plot the critical values
  q <- qt(1 - alpha / 2, n - 1)
  abline(v = c(q, -q), lwd = 2)
  axis(3, c(q, -q), c(expression(abs(t[alpha/2])), expression(-abs(t[alpha/2]))))

  # Add text annotations
  text(0.5 * (t0 + xmax), max(ycurve), substitute(paste(area, "=", va)))
  text(-0.5 * (t0 + xmax), max(ycurve), expression(area))

  return(list(q = q, pvalue = pv))
}
