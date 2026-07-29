#' @title Plot signed temporal HVarPart allocations
#' @description
#' Uses unstacked points and explicit zero/one reference lines so negative and
#' greater-than-one allocations remain interpretable.
#' @param data_summary Signed pooled temporal summary data.
#' @return A ggplot object.
plot_hvarpart_temporal_signed <- function(data_summary) {
  required <- c("analysis", "region", "age", "predictor", "pooled_allocation")
  assertthat::assert_that(
    is.data.frame(data_summary),
    all(required %in% names(data_summary)),
    msg = "Temporal HVarPart summary columns are missing."
  )

  data_plot <-
    data_summary |>
    dplyr::mutate(
      human_predictor = dplyr::recode(
        .data[["analysis"]],
        temporal_spd = "SPD",
        temporal_events = "Events"
      )
    )

  ggplot2::ggplot(
    data_plot,
    ggplot2::aes(
      x = .data[["age"]],
      y = .data[["pooled_allocation"]],
      colour = .data[["predictor"]],
      shape = .data[["human_predictor"]],
      linetype = .data[["human_predictor"]],
      group = interaction(
        .data[["predictor"]],
        .data[["human_predictor"]]
      )
    )
  ) +
    ggplot2::geom_hline(yintercept = c(0, 1), colour = "grey70") +
    ggplot2::geom_line(linewidth = line_size) +
    ggplot2::geom_point(size = point_size * 3) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["region"]]),
      ncol = 1
    ) +
    ggplot2::scale_x_reverse(
      labels = scales::label_number(scale = 0.001, accuracy = 0.5)
    ) +
    ggplot2::scale_colour_manual(values = palette_predictors) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = "Signed allocation of adjusted explained variation",
      colour = NULL,
      shape = NULL,
      linetype = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      strip.text.y = ggplot2::element_text(angle = 0)
    )
}
