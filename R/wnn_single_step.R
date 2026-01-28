#' WNN Single-Step Forecast
#'
#' Performs a single-step ahead forecast using Weighted Nearest Neighbors
#'
#' @param y Numeric vector. Target time series
#' @param X Numeric vector or matrix. Exogenous variables (optional)
#' @param m Integer. Embedding dimension (window size)
#' @param k Integer. Number of nearest neighbors
#' @param X_current Numeric vector or matrix. Current/future exogenous values (m rows)
#' @param scaling_params List. Pre-computed scaling parameters (optional)
#' @param distance Character. Distance metric: "euclidean" or "manhattan"
#' @param weight_func Character. Weight function: "inverse" or "inverse_squared"
#' @param temp_lags Integer. Number of temperature lags to use (default=1 for current only)
#'
#' @return List containing:
#' \itemize{
#'   \item forecast: Predicted value
#'   \item neighbors: Indices of nearest neighbors
#'   \item distances: Distances to neighbors
#'   \item weights: Normalized weights
#'   \item neighbor_values: Next values of neighbors
#'   \item n_exact_matches: Number of exact matches found
#' }
#'
#' @export
#' @examples
#' y <- sin(1:100 / 10) + rnorm(100, 0, 0.1)
#' result <- wnn_single_step(y, m = 10, k = 5)
#' print(result$forecast)
wnn_single_step <- function(y, X = NULL, m, k, X_current = NULL,
                            scaling_params = NULL,
                            distance = "euclidean",
                            weight_func = "inverse",
                            temp_lags = 1) {  # ← NEW PARAMETER

  library(FNN)

  # Validation
  n <- length(y)
  if (m >= n) stop("m must be smaller than length of y")
  if (k < 1) stop("k must be at least 1")

  if (!is.null(X)) {
    if (is.vector(X)) X <- matrix(X, ncol = 1)
    if (nrow(X) != n) stop("X must have same length as y")
  }

  # ------------------------------------------------------------
  # STEP 1: Create embeddings
  # ------------------------------------------------------------

  n_embeddings <- n - m

  embeddings_y <- matrix(0, nrow = n_embeddings, ncol = m)
  next_values <- numeric(n_embeddings)

  for (i in 1:n_embeddings) {
    t <- i + m - 1
    # x_t = (y_t, y_{t-1}, ..., y_{t-m+1}) - newest to oldest
    embeddings_y[i, ] <- y[t:(t - m + 1)]
    next_values[i] <- y[t + 1]
  }

  # Current embedding
  current_y <- y[n:(n - m + 1)]

  # Add exogenous variables if provided (IMPROVED VERSION)
  if (!is.null(X)) {
    n_exog <- ncol(X)
    # ← CHANGE: Use temp_lags instead of m for temperature
    embeddings_X <- matrix(0, nrow = n_embeddings, ncol = temp_lags * n_exog)

    for (i in 1:n_embeddings) {
      t <- i + m - 1
      for (j in 1:n_exog) {
        start_col <- (j - 1) * temp_lags + 1
        end_col <- j * temp_lags
        # ← CHANGE: Only take temp_lags most recent temperature values
        embeddings_X[i, start_col:end_col] <- X[t:(t - temp_lags + 1), j]
      }
    }

    # Current X embedding (IMPROVED VERSION)
    if (!is.null(X_current)) {
      if (is.vector(X_current)) X_current <- matrix(X_current, ncol = 1)

      # Build current_X to match embeddings_X structure
      n_exog <- ncol(X_current)
      current_X <- numeric(temp_lags * n_exog)

      for (j in 1:n_exog) {
        start_idx <- (j - 1) * temp_lags + 1
        end_idx <- j * temp_lags
        # ← CHANGE: Take only temp_lags most recent values
        current_X[start_idx:end_idx] <- X_current[1:temp_lags, j]
      }
    } else {
      # ← CHANGE: Use temp_lags instead of m
      current_X_mat <- X[n:(n - temp_lags + 1), , drop = FALSE]
      n_exog <- ncol(X)
      current_X <- numeric(temp_lags * n_exog)
      for (j in 1:n_exog) {
        start_idx <- (j - 1) * temp_lags + 1
        end_idx <- j * temp_lags
        current_X[start_idx:end_idx] <- current_X_mat[, j]
      }
    }

    # Combine embeddings
    embeddings_all <- cbind(embeddings_y, embeddings_X)
    current_all <- c(current_y, current_X)
  } else {
    embeddings_all <- embeddings_y
    current_all <- current_y
  }

  # ------------------------------------------------------------
  # STEP 1.5: Apply scaling if parameters provided
  # ------------------------------------------------------------

  if (!is.null(scaling_params)) {
    # Scale y part
    embeddings_all[, 1:m] <- (embeddings_all[, 1:m] - scaling_params$mean_y) /
      (scaling_params$sd_y + 1e-10)
    current_all[1:m] <- (current_all[1:m] - scaling_params$mean_y) /
      (scaling_params$sd_y + 1e-10)

    # Scale X part if exists
    if (!is.null(X)) {
      n_X_cols <- ncol(embeddings_all) - m
      for (j in 1:n_X_cols) {
        col_idx <- m + j
        embeddings_all[, col_idx] <- (embeddings_all[, col_idx] -
                                        scaling_params$mean_X[j]) /
          (scaling_params$sd_X[j] + 1e-10)
        current_all[col_idx] <- (current_all[col_idx] -
                                   scaling_params$mean_X[j]) /
          (scaling_params$sd_X[j] + 1e-10)
      }
    }
  }

  # ------------------------------------------------------------
  # STEP 2: Find k nearest neighbors
  # ------------------------------------------------------------

  knn_result <- get.knnx(
    data = embeddings_all,
    query = matrix(current_all, nrow = 1),
    k = min(k, nrow(embeddings_all))
  )

  neighbor_indices <- knn_result$nn.index[1, ]
  neighbor_distances <- knn_result$nn.dist[1, ]
  neighbor_next_values <- next_values[neighbor_indices]

  # ------------------------------------------------------------
  # STEP 3: Compute weights with zero-distance protection
  # ------------------------------------------------------------

  epsilon <- 1e-10  # Protection against division by zero

  if (weight_func == "inverse") {
    raw_weights <- 1 / (neighbor_distances + epsilon)
  } else if (weight_func == "inverse_squared") {
    raw_weights <- 1 / ((neighbor_distances + epsilon)^2)
  } else {
    stop("Unknown weight_func. Use 'inverse' or 'inverse_squared'")
  }

  normalized_weights <- raw_weights / sum(raw_weights)

  # ------------------------------------------------------------
  # STEP 4: Compute forecast
  # ------------------------------------------------------------

  forecast <- sum(normalized_weights * neighbor_next_values)

  # Check for exact matches
  n_exact_matches <- sum(neighbor_distances < 1e-8)

  # Return results
  list(
    forecast = forecast,
    neighbors = neighbor_indices,
    distances = neighbor_distances,
    weights = normalized_weights,
    neighbor_values = neighbor_next_values,
    n_exact_matches = n_exact_matches
  )
}
