#' Compute Scaling Parameters
#'
#' Calculates mean and standard deviation for standardization
#'
#' @param y Numeric vector. Target series
#' @param X Numeric vector or matrix. Exogenous variables (optional)
#' @param m Integer. Embedding dimension
#' @param temp_lags Integer. Number of temperature lags to use (default=1)
#'
#' @return List with scaling parameters
#' @export
#' @examples
#' y <- rnorm(100, 150, 20)
#' params <- wnn_compute_scaling(y, m = 10)
#' print(params)
wnn_compute_scaling <- function(y, X = NULL, m, temp_lags = 1) {
  n <- length(y)
  n_embeddings <- n - m

  # Create embeddings for y
  embeddings_y <- matrix(0, nrow = n_embeddings, ncol = m)
  for (i in 1:n_embeddings) {
    t <- i + m - 1
    embeddings_y[i, ] <- y[t:(t - m + 1)]
  }

  scaling_params <- list(
    mean_y = mean(embeddings_y),
    sd_y = sd(as.vector(embeddings_y))
  )

  # Create embeddings for X if provided (IMPROVED VERSION)
  if (!is.null(X)) {
    if (is.vector(X)) X <- matrix(X, ncol = 1)
    n_exog <- ncol(X)
    # ← CHANGE: Use temp_lags instead of m
    embeddings_X <- matrix(0, nrow = n_embeddings, ncol = temp_lags * n_exog)

    for (i in 1:n_embeddings) {
      t <- i + m - 1
      for (j in 1:n_exog) {
        start_col <- (j - 1) * temp_lags + 1
        end_col <- j * temp_lags
        # ← CHANGE: Only take temp_lags values
        embeddings_X[i, start_col:end_col] <- X[t:(t - temp_lags + 1), j]
      }
    }

    scaling_params$mean_X <- apply(embeddings_X, 2, mean)
    scaling_params$sd_X <- apply(embeddings_X, 2, sd)
  }

  return(scaling_params)
}
