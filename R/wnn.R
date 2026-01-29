#' Weighted Nearest Neighbors Forecasting
#'
#' Main wrapper function for WNN forecasting. Automatically handles both
#' single-step and multi-step forecasting with optional scaling.
#'
#' @param y Numeric vector. Target time series (e.g., electricity consumption)
#' @param X Numeric vector or matrix. Exogenous variables (e.g., temperature).
#'    Optional. Default = NULL
#' @param test_y Optional numeric vector of actual values for the forecast period to calculate metrics.
#' @param m Integer. Embedding dimension (window size).
#'    Recommended: 96 for daily patterns with 15-min data
#' @param k Integer. Number of nearest neighbors. Default = 10
#' @param h Integer. Forecast horizon (number of steps ahead). Default = 1
#' @param X_future Numeric vector or matrix. Future exogenous variables.
#'    Required if h > 1 and X is provided. Must have at least h rows.
#' @param scale Logical. Should embeddings be standardized? Default = TRUE
#' @param distance Character. Distance metric: "euclidean" or "manhattan".
#'    Default = "euclidean"
#' @param weight_func Character. Weighting function: "inverse" or "inverse_squared".
#'    Default = "inverse"
#' @param return_all Logical. Return detailed diagnostics? Default = FALSE
#' @param plot Logical. Should a plot of historical and forecasted values be shown? Default = TRUE
#' @param temp_lags Integer. Number of temperature lags to use. Default = 1 (current only)
#'
#' @return
#' If h = 1: List with forecast value and diagnostics
#' If h > 1 and return_all = FALSE: Numeric vector of forecasts
#' If h > 1 and return_all = TRUE: List with forecasts and detailed diagnostics
#'
#' @import ggplot2
#' @importFrom stats sd
#' @importFrom Metrics rmse mae
#' @export
wnn <- function(y, X = NULL, m, k = 10, h = 1,
                X_future = NULL,
                scale = TRUE,
                distance = "euclidean",
                weight_func = "inverse",
                return_all = FALSE,
                plot = TRUE,
                temp_lags = 1,
                test_y = NULL) {

  # ------------------------------------------------------------
  # 1. HEADER: Model Configuration
  # ------------------------------------------------------------
  cat("\n================================================\n")
  cat("  TRAINING WNN MODEL WITH PARAMETERS :\n")
  cat("------------------------------------------------\n")
  cat(sprintf(" \u2022 Horizon:    %d step(s)\n", h))
  cat(sprintf(" \u2022 Neighbors:  k = %d\n", k))
  cat(sprintf(" \u2022 Window (m): %d\n", m))
  cat(sprintf(" \u2022 Lags:        %d temp lag(s)\n", temp_lags))
  cat(sprintf(" \u2022 Scaling:     %s\n", ifelse(scale, "Enabled", "Disabled")))

  if (!is.numeric(y)) stop("y must be numeric")

  # ------------------------------------------------------------
  # 2. SCALING DISPLAY
  # ------------------------------------------------------------
  scaling_params <- NULL
  if (scale) {
    scaling_params <- wnn_compute_scaling(y = y, X = X, m = m, temp_lags = temp_lags)
    if (!is.null(X)) {
      cat("\n---- Scaling Statistics ----\n")
      cat(sprintf("  Consumption: mean = %.2f, sd = %.2f\n", scaling_params$mean_y, scaling_params$sd_y))
      cat(sprintf("  Exogenous:   mean = %.2f, sd = %.2f\n", scaling_params$mean_X[1], scaling_params$sd_X[1]))
    }
  }

  # ------------------------------------------------------------
  # 3. CALL ENGINE
  # ------------------------------------------------------------
  if (h == 1) {
    result <- wnn_single_step(y=y, X=X, m=m, k=k, X_current=X_future,
                              scaling_params=scaling_params, distance=distance,
                              weight_func=weight_func, temp_lags=temp_lags)
    # Extract forecast values for accuracy check
    forecasts_values <- result$forecast
  } else {
    result <- wnn_multi_step(y=y, X=X, m=m, k=k, h=h, X_future=X_future,
                             scaling_params=scaling_params, distance=distance,
                             weight_func=weight_func, return_all=return_all,
                             temp_lags=temp_lags)
    # Extract forecast values based on return_all
    forecasts_values <- if (return_all) result$forecasts else result
  }

  # ------------------------------------------------------------
  # 4. ACCURACY REPORT (Using Metrics package)
  # ------------------------------------------------------------
  if (!is.null(test_y)) {
    min_len <- min(length(test_y), length(forecasts_values))
    actual  <- test_y[1:min_len]
    pred    <- forecasts_values[1:min_len]

    # Use namespaced Metrics functions
    rmse_val <- Metrics::rmse(actual, pred)
    mae_val  <- Metrics::mae(actual, pred)

    cat("\n------------------------------------------------\n")
    cat("  PERFORMANCE METRICS (vs Test Set)\n")
    cat("------------------------------------------------\n")
    cat(sprintf("   RMSE:  %.2f\n", rmse_val))
    cat(sprintf("   MAE:   %.2f\n", mae_val))

    if (return_all && is.list(result)) {
      result$metrics <- list(rmse = rmse_val, mae = mae_val)
    }
  }

  # ------------------------------------------------------------
  # 5. CORRECTED PLOTTING LOGIC (Using Namespacing)
  # ------------------------------------------------------------
  if (plot) {
    if (requireNamespace("ggplot2", quietly = TRUE)) {

      n_total <- length(y)
      h_steps <- length(forecasts_values)

      n_context <- min(5 * 96, n_total)
      context_start <- n_total - n_context + 1

      df_train <- data.frame(
        Time = (context_start:n_total) / 96,
        Value = as.numeric(y[context_start:n_total]),
        Series = "Train data"
      )

      df_forecast <- data.frame(
        Time = (n_total + 1):(n_total + h_steps) / 96,
        Value = as.numeric(forecasts_values),
        Series = "WNN Forecast"
      )

      p <- ggplot2::ggplot() +
        ggplot2::geom_line(data = df_train,
                           ggplot2::aes(x = Time, y = Value, color = Series),
                           linewidth = 0.7)

      if (!is.null(test_y)) {
        df_actual <- data.frame(
          Time = (n_total + 1):(n_total + length(test_y)) / 96,
          Value = as.numeric(test_y),
          Series = "Test data"
        )
        p <- p + ggplot2::geom_line(data = df_actual,
                                    ggplot2::aes(x = Time, y = Value, color = Series),
                                    linetype = "dashed",
                                    linewidth = 0.5)
      }

      p <- p + ggplot2::geom_line(data = df_forecast,
                                  ggplot2::aes(x = Time, y = Value, color = Series),
                                  linewidth = 0.7)

      p <- p + ggplot2::geom_vline(xintercept = n_total / 96,
                                   linetype = "dotted",
                                   color = "gray50",
                                   alpha = 0.8)

      color_map <- c("Train data" = "black", "WNN Forecast" = "red", "Test data" = "black")

      p <- p +
        ggplot2::scale_color_manual(values = color_map) +
        ggplot2::labs(
          title = "WNN Forecast",
          subtitle = if (!is.null(test_y)) {
            sprintf("Model Performance | RMSE: %.2f | MAE: %.2f", rmse_val, mae_val)
          } else {
            sprintf("Horizon: %d intervals | Neighbors: k=%d", h_steps, k)
          },
          x = "Time (Days)",
          y = "Power Consumption (kW)",
          color = NULL
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          legend.position = "bottom",
          panel.grid.minor = ggplot2::element_blank()
        )

      print(p)

      if (return_all && is.list(result)) { result$plot <- p }
    }
  }

  return(result)
}
