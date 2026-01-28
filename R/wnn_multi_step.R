#' WNN Multi-Step Forecast
#'
#' Performs multi-step ahead forecast using recursive strategy
#'
#' @param y Numeric vector. Target time series
#' @param X Numeric vector or matrix. Exogenous variables (optional)
#' @param m Integer. Embedding dimension
#' @param k Integer. Number of nearest neighbors
#' @param h Integer. Forecast horizon (number of steps ahead)
#' @param X_future Numeric vector or matrix. Future exogenous values (h rows)
#' @param scaling_params List. Pre-computed scaling parameters (optional)
#' @param distance Character. Distance metric
#' @param weight_func Character. Weight function
#' @param return_all Logical. Return detailed diagnostics for each step?
#' @param temp_lags Integer. Number of temperature lags to use (default=1)
#'
#' @return
#' If return_all = FALSE: Numeric vector of h forecasts
#' If return_all = TRUE: List with forecasts and diagnostics for each step
#'
#' @export
#' @examples
#' y <- sin(1:100 / 10) + rnorm(100, 0, 0.1)
#' forecasts <- wnn_multi_step(y, m = 10, k = 5, h = 10)
#' plot(forecasts, type = "l")
wnn_multi_step <- function(y, X = NULL, m, k, h,
                           X_future = NULL,
                           scaling_params = NULL,
                           distance = "euclidean",
                           weight_func = "inverse",
                           return_all = FALSE,
                           temp_lags = 1) {  # ← NEW PARAMETER

  # Validation
  if (h < 1) stop("h must be at least 1")
  if (!is.null(X) && is.null(X_future)) {
    stop("X_future is required when X is provided")
  }
  if (!is.null(X_future)) {
    if (is.vector(X_future)) X_future <- matrix(X_future, ncol = 1)
    if (nrow(X_future) < h) {
      stop("X_future must have at least h rows")
    }
  }

  # Initialize
  forecasts <- numeric(h)
  diagnostics <- if (return_all) vector("list", h) else NULL
  y_extended <- y
  X_extended <- X

  # Recursive forecasting loop
  for (i in 1:h) {
    # Prepare X_current for this iteration
    if (!is.null(X)) {
      # Simply take the last m rows from X_extended
      n_ext <- nrow(X_extended)
      X_current <- X_extended[(n_ext - m + 1):n_ext, , drop = FALSE]

      # Reverse to get [newest, ..., oldest]
      X_current <- X_current[m:1, , drop = FALSE]
    } else {
      X_current <- NULL
    }

    # Make single-step forecast
    result <- wnn_single_step(
      y = y_extended,
      X = X_extended,
      m = m,
      k = k,
      X_current = X_current,
      scaling_params = scaling_params,
      distance = distance,
      weight_func = weight_func,
      temp_lags = temp_lags  # ← PASS NEW PARAMETER
    )

    # Store results
    forecasts[i] <- result$forecast
    if (return_all) {
      diagnostics[[i]] <- result
    }

    # Update for next iteration
    y_extended <- c(y_extended, result$forecast)
    if (!is.null(X)) {
      X_extended <- rbind(X_extended, X_future[i, , drop = FALSE])
    }
  }

  # Return results
  if (return_all) {
    return(list(
      forecasts = forecasts,
      diagnostics = diagnostics
    ))
  } else {
    return(forecasts)
  }
}
