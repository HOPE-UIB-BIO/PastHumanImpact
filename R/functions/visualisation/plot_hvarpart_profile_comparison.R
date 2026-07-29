#' @title Plot HVarPart sensitivity-profile comparisons
#' @description Plot pooled predictor allocations under the signed primary
#' contract and both fixed sensitivity profiles.
#' @param data_profiles Output from
#' `compare_hvarpart_importance_profiles()` grouped by analysis.
#' @return A ggplot object.
plot_hvarpart_profile_comparison <- function(data_profiles) {
  required <- c(
    "analysis", "predictor", "profile", "pooled_allocation", "n_models"
  )
  assertthat::assert_that(
    is.data.frame(data_profiles),
    all(required %in% names(data_profiles)),
    msg = "Profile comparison columns are missing."
  )

  data_profiles |>
    dplyr::mutate(
      profile = factor(
        .data[["profile"]],
        levels = c("signed", "zero_truncated", "exclude_negative")
      )
    ) |>
    ggplot2::ggplot(
      ggplot2::aes(
        x = .data[["profile"]],
        y = .data[["pooled_allocation"]],
        colour = .data[["predictor"]],
        group = .data[["predictor"]]
      )
    ) +
    ggplot2::geom_hline(yintercept = c(0, 1), colour = "grey75") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = point_size * 2) +
    ggplot2::facet_wrap(ggplot2::vars(.data[["analysis"]]), scales = "free_y") +
    ggplot2::scale_colour_manual(values = palette_predictors) +
    ggplot2::labs(
      x = "Importance profile",
      y = "Allocation of adjusted explained variation",
      colour = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 35, hjust = 1))
}
