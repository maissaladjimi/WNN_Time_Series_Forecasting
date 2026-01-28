#' Weighted Nearest Neighbors Forecasting
#'
#' Main wrapper function for WNN forecasting. Automatically handles both
#' single-step and multi-step forecasting with optional scaling.
#'
#' @param y Numeric vector. Target time series (e.g., electricity consumption)
#' @param X Numeric vector or matrix. Exogenous variables (e.g., temperature).
#'   Optional. Default = NULL
#' @param m Integer. Embedding dimension (window size).
#'   Recommended: 96 for daily patterns with 15-min data
#' @param k Integer. Number of nearest neighbors. Default = 10
#' @param h Integer. Forecast horizon (number of steps ahead). Default = 1
#' @param X_future Numeric vector or matrix. Future exogenous variables.
#'   Required if h > 1 and X is provided. Must have at least h rows.
#' @param scale Logical. Should embeddings be standardized? Default = TRUE
#' @param distance Character. Distance metric: "euclidean" or "manhattan".
#'   Default = "euclidean"
#' @param weight_func Character. Weighting function: "inverse" or "inverse_squared".
#'   Default = "inverse"
#' @param return_all Logical. Return detailed diagnostics? Default = FALSE
#' @param plot Logical. Should a plot of historical and forecasted values be shown? Default = TRUE
#' @param temp_lags Integer. Number of temperature lags to use. Default = 1 (current only)
#'
#' @return
#' If h = 1: List with forecast value and diagnostics
#' If h > 1 and return_all = FALSE: Numeric vector of forecasts
#' If h > 1 and return_all = TRUE: List with forecasts and detailed diagnostics
#'
#' @export
wnn <- function(y, X = NULL, m, k = 10, h = 1,
                X_future = NULL,
                scale = TRUE,
                distance = "euclidean",
                weight_func = "inverse",
                return_all = FALSE,
                plot = TRUE,
                temp_lags = 1) {  # ← NEW PARAMETER

  # ============================================================
  # INPUT VALIDATION
  # ============================================================

  if (!is.numeric(y)) stop("y must be numeric")
  if (length(y) < m + 1) stop("Insufficient data: need at least m + 1 observations")
  if (m < 1) stop("m must be at least 1")
  if (k < 1) stop("k must be at least 1")
  if (h < 1) stop("h must be at least 1")

  # Validate exogenous variables
  if (!is.null(X)) {
    if (is.vector(X)) X <- matrix(X, ncol = 1)
    if (nrow(X) != length(y)) {
      stop("X must have same number of rows as length of y")
    }
  }

  # Validate future exogenous variables for multi-step
  if (h > 1 && !is.null(X)) {
    if (is.null(X_future)) {
      stop("X_future is required for multi-step forecasting with exogenous variables")
    }
    if (is.vector(X_future)) X_future <- matrix(X_future, ncol = 1)
    if (nrow(X_future) < h) {
      stop(sprintf("X_future must have at least %d rows (h = %d)", h, h))
    }
    if (ncol(X_future) != ncol(X)) {
      stop("X_future must have same number of columns as X")
    }
  }

  # ============================================================
  # COMPUTE SCALING PARAMETERS
  # ============================================================

  scaling_params <- NULL

  if (scale) {
    # ← CHANGE: Pass temp_lags to scaling function
    scaling_params <- wnn_compute_scaling(y = y, X = X, m = m, temp_lags = temp_lags)

    if (!is.null(X)) {
      cat("Scaling parameters:\n")
      cat(sprintf("  Consumption: mean = %.2f, sd = %.2f\n",
                  scaling_params$mean_y, scaling_params$sd_y))
      cat(sprintf("  Exogenous vars: mean = %.2f, sd = %.2f (first var)\n",
                  scaling_params$mean_X[1], scaling_params$sd_X[1]))
      cat(sprintf("  Temperature lags used: %d\n", temp_lags))
    }
  }

  # ============================================================
  # CALL APPROPRIATE FUNCTION
  # ============================================================

  if (h == 1) {
    # Single-step forecast
    result <- wnn_single_step(
      y = y,
      X = X,
      m = m,
      k = k,
      X_current = if (!is.null(X_future)) X_future[1:m, , drop = FALSE] else NULL,
      scaling_params = scaling_params,
      distance = distance,
      weight_func = weight_func,
      temp_lags = temp_lags  # ← PASS NEW PARAMETER
    )

    return(result)

  } else {
    # Multi-step forecast
    result <- wnn_multi_step(
      y = y,
      X = X,
      m = m,
      k = k,
      h = h,
      X_future = X_future,
      scaling_params = scaling_params,
      distance = distance,
      weight_func = weight_func,
      return_all = return_all,
      temp_lags = temp_lags  # ← PASS NEW PARAMETER
    )

    # ============================================================
    # PLOTTING
    # ============================================================

    # Extract numeric forecasts for plotting
    if (return_all) {
      forecasts_values <- result$forecasts
    } else {
      forecasts_values <- result
    }

    if (plot) {
      # Plot FULL training data + forecast

      # Create day labels (assuming 96 observations per day)
      hist_days <- seq_along(y) / 96
      forecast_days <- (length(y) + seq_along(forecasts_values)) / 96

      plot_df <- data.frame(
        Day = c(hist_days, forecast_days),
        Power = c(y, forecasts_values),
        Type = c(rep("Historical", length(y)),
                 rep("Forecast", length(forecasts_values)))
      )

      if (requireNamespace("ggplot2", quietly = TRUE)) {
        print(
          ggplot2::ggplot(plot_df, ggplot2::aes(x = Day, y = Power, color = Type)) +
            ggplot2::geom_line(linewidth = 0.8) +
            ggplot2::scale_color_manual(values = c("Historical" = "blue", "Forecast" = "red")) +
            ggplot2::labs(title = sprintf("WNN Forecast (m=%d, k=%d, temp_lags=%d)", m, k, temp_lags),
                          x = "Day", y = "Power (kW)") +
            ggplot2::theme_minimal()
        )
      }
    }

    return(result)
  }
}
