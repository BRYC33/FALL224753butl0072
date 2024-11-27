#' Bootstrap Confidence Interval and Histogram Plot
#'
#' @param iter An integer specifying the number of bootstrap iterations. Default is 10000.
#' @param x A numeric vector containing the sample data to bootstrap.
#' @param fun A character string or function for the statistic to compute for each bootstrap sample. Default is "mean".
#' @param alpha A numeric value for the significance level. Default is 0.05 (for a 95\% confidence interval).
#' @param cx A numeric value specifying the scaling factor for text size in confidence interval annotations. Default is 1.5.
#' @param xlab A character string for the label of the x-axis in the histogram. Default is "Statistic".
#' @param ylab A character string for the label of the y-axis in the histogram. Default is "Density".
#' @param ... Additional graphical parameters to be passed to \code{hist()}, such as \code{col} for color.
#'
#' @return
#' @examples
#' # Example usage of myboot2 function
#' set.seed(39)
#' sam <- rnorm(25, mean = 25, sd = 10)
#' myboot2(iter = 10000, x = sam, fun = "mean", alpha = 0.05, xlab = "Mean", ylab = "Density", col = "Purple", cx = 1.5)
#'
#' @export
myboot2 <- function(iter=10000, x, fun="mean", alpha=0.05, cx=1.5, xlab="Statistic", ylab="Density", ...) {
  n <- length(x)  # Sample size
  y <- sample(x, n * iter, replace=TRUE)  # Resampling with replacement
  rs.mat <- matrix(y, nrow=n, ncol=iter, byrow=TRUE)
  xstat <- apply(rs.mat, 2, fun)  # Calculate the statistic for each resample
  ci <- quantile(xstat, c(alpha/2, 1-alpha/2))  # Confidence interval

  # Plot histogram with specified x and y labels
  para <- hist(xstat, freq=FALSE, las=1,
               main=paste("Histogram of Bootstrap Sample Statistics", "\n",
                          "alpha=", alpha, " iter=", iter, sep=""),
               xlab=xlab, ylab=ylab, ...)

  mat <- matrix(x, nrow=length(x), ncol=1, byrow=TRUE)
  pte <- apply(mat, 2, fun)  # Point estimate

  # Add CI and point estimate lines to histogram
  abline(v=pte, lwd=3, col="Black")
  segments(ci[1], 0, ci[2], 0, lwd=4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep=""), col="Red", cex=cx, pos=4)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep=""), col="Red", cex=cx, pos=4)
  text(pte, max(para$density) / 2, round(pte, 2), cex=cx)

  invisible(list(ci=ci, fun=fun, x=x, xstat=xstat))  # Return list with CI and function info
}
