#' Compare Two Forecasts Visually and Summarize
#'
#' Since actual values are not available, this function compares two forecasts
#' by plotting them and their differences, and printing simple summary stats.
#'
#' @param forecast1 Numeric vector. Forecast from first model (e.g., Part 1)
#' @param forecast2 Numeric vector. Forecast from second model (e.g., WNN)
#' @param labels Character vector of length 2. Names for the forecasts
#' @return NULL (produces plots and prints summaries)
#' @export
compare_forecasts <- function(forecast1, forecast2, labels = c("Model 1", "Model 2")) {

  # Check lengths
  n <- length(forecast1)
  if(length(forecast2) != n) stop("Both forecasts must have the same length")

  # 1️⃣ Plot forecasts together
  plot(1:n, forecast1, type = "l", col = "red", lwd = 2,
       ylim = range(c(forecast1, forecast2)),
       xlab = "Step", ylab = "Forecast",
       main = "Forecast Comparison")
  lines(1:n, forecast2, col = "blue", lwd = 2)
  legend("topright", legend = labels, col = c("red","blue"), lwd = 2)

  # 2️⃣ Plot forecast differences
  diff_forecast <- forecast2 - forecast1
  plot(1:n, diff_forecast, type = "l", col = "purple", lwd = 2,
       xlab = "Step", ylab = "Difference",
       main = paste("Difference:", labels[2], "-", labels[1]))
  abline(h = 0, lty = 2, col = "black")

  # 3️⃣ Print basic summary stats
  cat("Summary statistics:\n")
  cat(sprintf("%s: mean=%.2f, sd=%.2f, min=%.2f, max=%.2f\n",
              labels[1], mean(forecast1), sd(forecast1), min(forecast1), max(forecast1)))
  cat(sprintf("%s: mean=%.2f, sd=%.2f, min=%.2f, max=%.2f\n",
              labels[2], mean(forecast2), sd(forecast2), min(forecast2), max(forecast2)))

  cat(sprintf("Difference (Model 2 - Model 1): mean=%.2f, sd=%.2f, min=%.2f, max=%.2f\n",
              mean(diff_forecast), sd(diff_forecast), min(diff_forecast), max(diff_forecast)))
}
