#' @title Plot HVarPart sensitivity-profile comparisons
#' @description
#' Shows every model allocation for the zero-truncated, signed, and
#' negative-exclusion profiles, with the pooled ratio-of-sums estimate
#' overlaid for each analysis and predictor.
#' @param data_profiles Output from
#' `compare_hvarpart_importance_profiles()` containing `analysis` and `model`
#' aggregation levels.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' plot_hvarpart_profile_comparison(profile_summary)
#' }
plot_hvarpart_profile_comparison <- function(data_profiles) {
  required <- c(
    "analysis", "aggregation_level", "model_id", "predictor", "profile",
    "pooled_allocation", "n_models"
  )
  assertthat::assert_that(
    is.data.frame(data_profiles),
    all(required %in% names(data_profiles)),
    msg = "Profile comparison columns are missing."
  )

  data_plot <-
    data_profiles |>
    dplyr::filter(
      .data[["aggregation_level"]] %in% c("analysis", "model")
    ) |>
    dplyr::mutate(
      profile = factor(
        .data[["profile"]],
        levels = c("zero_truncated", "signed", "exclude_negative"),
        labels = c(
          "Zero-\ntruncated\n(main)",
          "Signed",
          "Exclude-\nnegative"
        )
      )
    )
  data_models <-
    data_plot |>
    dplyr::filter(.data[["aggregation_level"]] == "model")
  data_pooled <-
    data_plot |>
    dplyr::filter(.data[["aggregation_level"]] == "analysis")

  assertthat::assert_that(
    nrow(data_models) > 0L,
    nrow(data_pooled) > 0L,
    msg = "Profile comparison requires model and analysis rows."
  )

  result <-
    data_models |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["profile"]],
        y = .data[["pooled_allocation"]],
        colour = .data[["predictor"]],
        fill = .data[["predictor"]]
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      colour = "grey75"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "grey75",
      linetype = 2
    ) +
    ggplot2::geom_violin(
      alpha = 0.15,
      linewidth = line_size,
      position = ggplot2::position_dodge(width = 0.7),
      scale = "width",
      trim = TRUE
    ) +
    ggplot2::geom_point(
      alpha = 0.2,
      size = point_size,
      position = ggplot2::position_jitterdodge(
        jitter.width = 0.12,
        dodge.width = 0.7,
        seed = set_seed
      ),
      show.legend = FALSE
    ) +
    ggplot2::geom_line(
      data = data_pooled,
      mapping = ggplot2::aes(group = .data[["predictor"]]),
      linewidth = line_size * 3,
      position = ggplot2::position_dodge(width = 0.7)
    ) +
    ggplot2::geom_point(
      data = data_pooled,
      mapping = ggplot2::aes(shape = "Pooled estimate"),
      colour = common_gray,
      size = point_size * 3,
      stroke = line_size * 4,
      position = ggplot2::position_dodge(width = 0.7)
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["analysis"]]),
      scales = "free_y"
    ) +
    ggplot2::scale_colour_manual(
      "Predictor",
      values = palette_predictors
    ) +
    ggplot2::scale_fill_manual(
      "Predictor",
      values = palette_predictors
    ) +
    ggplot2::scale_shape_manual(
      "Summary",
      values = c("Pooled estimate" = 21)
    ) +
    ggplot2::labs(
      x = "Importance profile",
      y = "Allocation of adjusted explained variation",
      colour = NULL,
      fill = NULL,
      shape = NULL
    ) +
    ggplot2::coord_cartesian(ylim = c(-0.5, 1.5)) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = 0,
        hjust = 0.5,
        size = text_size * 0.8
      ),
      legend.position = "bottom"
    )

  return(result)
}
