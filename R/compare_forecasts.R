#' Compare WNN Forecast with External Forecast
#'
#' @param external_forecast Numeric vector. The benchmark forecast (e.g., SVM).
#' @param wnn_forecast Numeric vector. Pre-calculated WNN results.
#' @param y Numeric vector. Historical power data (used for context/stats).
#' @param method_name Character. Label for the external method (e.g., "SVM").
#'
#' @import ggplot2
#' @importFrom utils tail
#' @importFrom stats sd cor
#' @export
compare_forecasts <- function(external_forecast,
                              wnn_forecast,
                              y = NULL,
                              method_name = "External") {

  # 1. Validation & Alignment
  if (missing(wnn_forecast) || is.null(wnn_forecast)) stop("wnn_forecast is required.")

  # Use utils::tail
  ext_vec <- as.numeric(utils::tail(external_forecast, 96))
  wnn_vec <- as.numeric(utils::tail(wnn_forecast, 96))

  # 2. Statistics & Patterns
  if (!is.null(y)) {
    n_days <- floor(length(y) / 96)
    y_matrix <- matrix(y[1:(n_days * 96)], nrow = 96)
    typical_pattern <- base::rowMeans(y_matrix)
    last_day <- utils::tail(y, 96)
  } else {
    typical_pattern <- last_day <- rep(NA, 96)
  }

  # Use stats::sd and base::mean
  get_stats <- function(v) {
    c(Mean = mean(v, na.rm=TRUE),
      SD = stats::sd(v, na.rm=TRUE),
      Min = min(v, na.rm=TRUE),
      Max = max(v, na.rm=TRUE),
      CV = stats::sd(v, na.rm=TRUE)/mean(v, na.rm=TRUE))
  }

  summary_table <- data.frame(
    Metric = c("Mean (kW)", "Std Dev", "Min", "Max", "Coeff Var", "Pattern Cor"),
    Historical = c(round(get_stats(last_day), 3), 1.000),
    WNN        = c(round(get_stats(wnn_vec), 3), round(stats::cor(wnn_vec, typical_pattern), 3)),
    External   = c(round(get_stats(ext_vec), 3), round(stats::cor(ext_vec, typical_pattern), 3))
  )
  colnames(summary_table)[4] <- method_name

  # 3. Console Report
  cat("\n================================================\n")
  cat(sprintf("  COMPARISON SUMMARY: WNN vs %s\n", toupper(method_name)))
  cat("------------------------------------------------\n")
  print(summary_table, row.names = FALSE)
  cat("================================================\n\n")

  # 4. VISUALIZATIONS
  plots <- list()
  if (requireNamespace("ggplot2", quietly = TRUE) && !is.null(y)) {
    n_hist <- length(y)
    n_context <- min(4 * 96, n_hist) # Show 4 days of context

    hist_days <- ((n_hist - n_context + 1):n_hist)/96
    forecast_days <- ((n_hist + 1):(n_hist + 96))/96

    plot_df <- data.frame(
      Day = c(hist_days, rep(forecast_days, 2)),
      Power = c(utils::tail(y, n_context), wnn_vec, ext_vec),
      Type = factor(c(rep("Historical", n_context), rep("WNN", 96), rep(method_name, 96)),
                    levels = c("Historical", "WNN", method_name))
    )

    my_colors <- c("Historical" = "black", "WNN" = "forestgreen")
    my_colors[method_name] <- "red"

    # --- Plot A: Comparison ---
    plots$comparison <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Day, y = Power, color = Type)) +
      ggplot2::geom_line(ggplot2::aes(linetype = Type), linewidth = 0.8) +
      ggplot2::scale_color_manual(values = my_colors) +
      ggplot2::geom_vline(xintercept = n_hist/96, linetype = "dotted") +
      ggplot2::labs(title = paste("Forecast Comparison:", method_name, "vs WNN"),
                    y = "Power (kW)", x = "Day") +
      ggplot2::theme_minimal()

    # --- Plot B: Residuals ---
    diffs <- wnn_vec - ext_vec

    res_df <- data.frame(
      Hour = (1:96 - 1) * 0.25,
      Difference = diffs
    )

    plots$difference <- ggplot2::ggplot(res_df, ggplot2::aes(x = Hour, y = Difference)) +
      ggplot2::geom_line(color = "purple", linewidth = 0.8) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      ggplot2::scale_x_continuous(
        breaks = seq(0, 24, by = 3),
        limits = c(0, 24),
        labels = function(x) paste0(x, ":00")
      ) +
      ggplot2::labs(
        title = paste("Residuals (WNN -", method_name, ")"),
        subtitle = "Positive: WNN higher | Negative: External higher",
        x = "Time of Day (Hours)",
        y = "Difference (kW)"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor.x = ggplot2::element_blank())
  }

  return(list(summary_table = summary_table, plots = plots))
}
