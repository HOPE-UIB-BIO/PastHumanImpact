#' @title Plot paired sequence adjusted R-squared comparisons
#' @description
#' Compare canonical SPD-only and collinearity-filtered joint-human HVarPart
#' adjusted R-squared values for every sequence estimated by both analyses.
#' @param comparison_long Long paired comparison table returned by
#' `prepare_sequence_r2_predictor_overview()`.
#' @param r2_summary Metric-level comparison summary returned by the same
#' preparation function.
#' @return A faceted `ggplot` with one point per matched sequence and a 1:1
#' reference line.
#' @examples
#' \dontrun{
#' plot_sequence_r2_comparison(
#'   overview[["comparison_long"]], overview[["r2_summary"]]
#' )
#' }
plot_sequence_r2_comparison <- function(comparison_long, r2_summary) {
  assertthat::assert_that(
    is.data.frame(comparison_long), is.data.frame(r2_summary),
    all(c(
      "dataset_id", "metric", "spd_only", "filtered_joint", "difference"
    ) %in% names(comparison_long)),
    all(c(
      "metric", "n_sequences", "correlation", "median_difference"
    ) %in% names(r2_summary)),
    msg = "Sequence R-squared plot inputs do not satisfy the contract."
  )
  annotations <- r2_summary |>
    dplyr::mutate(
      label = sprintf(
        "n = %s\nr = %.2f\nmedian Δ = %+.3f",
        .data[["n_sequences"]], .data[["correlation"]],
        .data[["median_difference"]]
      ),
      x = -Inf,
      y = Inf
    )

  res <- ggplot2::ggplot(
    comparison_long,
    ggplot2::aes(x = .data[["spd_only"]], y = .data[["filtered_joint"]])
  ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#D0D0D0", linewidth = 0.3) +
    ggplot2::geom_vline(xintercept = 0, colour = "#D0D0D0", linewidth = 0.3) +
    ggplot2::geom_abline(
      slope = 1, intercept = 0, colour = common_gray,
      linewidth = 0.55, linetype = "22"
    ) +
    ggplot2::geom_point(
      shape = 21, size = 1.8, stroke = 0.2,
      colour = "#333333", fill = "#8E9A9A", alpha = 0.48
    ) +
    ggplot2::geom_label(
      data = annotations,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
      inherit.aes = FALSE,
      hjust = -0.08, vjust = 1.08, size = 3.2,
      linewidth = 0, fill = scales::alpha("white", 0.82)
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["metric"]]), ncol = 2) +
    ggplot2::coord_equal() +
    ggplot2::labs(
      x = "Canonical SPD-only model",
      y = "Filtered joint-human model",
      title = "Sequence-level adjusted R² comparison",
      subtitle = paste(
        "Each point is one sequence estimated in both analyses;",
        "the dashed line marks identical values"
      )
    ) +
    ggplot2::theme_bw(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(
        colour = "#ECECEC", linewidth = 0.3
      ),
      strip.background = ggplot2::element_rect(
        fill = "#F2F2F2", colour = common_gray
      ),
      plot.title.position = "plot"
    )
  return(res)
}
