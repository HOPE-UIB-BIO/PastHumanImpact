#' Plot region-specific human-event inclusion spatial sensitivity
#'
#' @param data_all_available Long dataset-level source table.
#' @param data_summary Matched distribution summary table.
#' @param event_human_colour Orange used for event-only human results.
#' @param metric_profile HVAR presentation profile. The default
#'   `"zero_truncated"` shows the renormalised balance of positive human and
#'   climate contributions. `"untruncated_signed"` is a supplementary
#'   adjusted-R-squared diagnostic.
#'
#' @return A two-panel patchwork figure.
#'
#' @export
plot_human_event_spatial_comparison <- function(
  data_all_available,
  data_summary,
  event_human_colour = "#DC702E",
  metric_profile = c("zero_truncated", "untruncated_signed")
) {
  metric_profile <-
    match.arg(metric_profile)

  metric_column <-
    if (metric_profile == "zero_truncated") {
      "zero_balance"
    } else {
      "signed_difference"
    }

  balance_label <-
    if (metric_profile == "zero_truncated") {
      "Zero-truncated human-climate\nimportance balance"
    } else {
      paste(
        "Untruncated signed human-climate",
        "hierarchical contribution"
      )
    }

  required_all <- c(
    "cohort",
    "proxy_variant",
    "dataset_id",
    "region",
    "status",
    metric_column
  )
  required_summary <- c(
    "cohort",
    "summary_level",
    "region",
    "metric",
    "contrast",
    "median",
    "lower_quartile",
    "upper_quartile"
  )
  assertthat::assert_that(
    is.data.frame(data_all_available),
    all(required_all %in% names(data_all_available)),
    is.data.frame(data_summary),
    all(required_summary %in% names(data_summary)),
    msg = "Human-event spatial figure inputs do not satisfy the contract."
  )

  region_levels <-
    c("North America", "Latin America", "Europe", "Asia", "Oceania")
  region_labels <- region_labeller
  region_labels[["Latin America"]] <- "Central &\nSouth America"
  proxy_levels <- c("spd", "spd_events", "events")
  proxy_labels <- c(
    spd = "SPD",
    spd_events = "Combined",
    events = "Events only"
  )
  proxy_axis_labels <- c(
    spd = "SPD\nonly",
    spd_events = "Combined",
    events = "Events\nonly"
  )
  data_plot <-
    data_all_available |>
    dplyr::filter(
      .data[["cohort"]] == "as_coded",
      .data[["status"]] %in% c(
        "estimated",
        "estimated_residual_temporal_dependence"
      ),
      is.finite(.data[[metric_column]])
    ) |>
    dplyr::group_by(.data[["dataset_id"]]) |>
    dplyr::filter(dplyr::n_distinct(.data[["proxy_variant"]]) == 3L) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      proxy_variant = factor(.data[["proxy_variant"]],
        levels = proxy_levels),
      region = factor(.data[["region"]], levels = region_levels)
    )

  plot_pairs <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["proxy_variant"]],
        y = .data[[metric_column]]
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = common_gray,
      linewidth = line_size * 2
    ) +
    ggplot2::geom_line(
      ggplot2::aes(group = .data[["dataset_id"]]),
      colour = common_gray,
      alpha = 0.08,
      linewidth = line_size * 2.5
    ) +
    ggplot2::geom_point(
      ggplot2::aes(
        shape = .data[["proxy_variant"]],
        fill = .data[[metric_column]],
        colour = .data[["proxy_variant"]]
      ),
      alpha = 0.65,
      size = point_size * 1.7,
      stroke = line_size * 2.5
    ) +
    ggplot2::facet_wrap(
      ggplot2::vars(.data[["region"]]),
      nrow = 1,
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(region_labels)
      )
    ) +
    ggplot2::scale_x_discrete(labels = proxy_axis_labels) +
    ggplot2::scale_y_continuous(
      limits = c(-1, 1),
      breaks = c(-1, -0.5, 0, 0.5, 1)
    ) +
    ggplot2::scale_shape_manual(
      values = c(spd = 21, spd_events = 24, events = 22),
      labels = proxy_labels
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        spd = palette_predictors[["human"]],
        spd_events = palette_predictors[["human"]],
        events = event_human_colour
      ),
      labels = proxy_labels
    ) +
    ggplot2::scale_fill_gradient2(
      low = palette_predictors[["climate"]],
      mid = "#F2F2F2",
      high = palette_predictors[["human"]],
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, 0, 1),
      oob = scales::squish
    ) +
    ggplot2::labs(
      x = NULL,
      y = balance_label,
      shape = "Human-impact proxy",
      colour = "Human-impact proxy",
      fill = balance_label
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        size = text_size * 0.55,
        lineheight = 0.9
      ),
      panel.spacing = grid::unit(2, "mm"),
      legend.position = "bottom",
      legend.box = "vertical"
    )

  contrast_labels <- c(
    spd_events_minus_spd = "Combined minus SPD",
    events_minus_spd = "Events only minus SPD"
  )
  data_summary_plot <-
    data_summary |>
    dplyr::filter(
      .data[["cohort"]] == "as_coded",
      .data[["summary_level"]] == "continent",
      .data[["metric"]] == .env[["metric_column"]],
      is.finite(.data[["median"]])
    ) |>
    dplyr::mutate(
      region = factor(.data[["region"]], levels = rev(region_levels)),
      contrast = factor(.data[["contrast"]], names(contrast_labels))
    )
  plot_summary <-
    ggplot2::ggplot(
      data_summary_plot,
      ggplot2::aes(
        x = .data[["median"]],
        y = .data[["region"]],
        colour = .data[["contrast"]],
        shape = .data[["contrast"]]
      )
    ) +
    ggplot2::geom_vline(
      xintercept = 0,
      colour = common_gray,
      linewidth = line_size * 2
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        xmin = .data[["lower_quartile"]],
        xmax = .data[["upper_quartile"]]
      ),
      orientation = "y",
      width = 0,
      position = ggplot2::position_dodge(width = 0.45),
      linewidth = line_size * 4
    ) +
    ggplot2::geom_point(
      position = ggplot2::position_dodge(width = 0.45),
      size = point_size * 2.4
    ) +
    ggplot2::scale_y_discrete(labels = region_labels) +
    ggplot2::scale_x_continuous(
      limits = c(-2, 2),
      breaks = c(-2, -1, 0, 1, 2)
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        spd_events_minus_spd = palette_predictors[["human"]],
        events_minus_spd = event_human_colour
      ),
      labels = contrast_labels
    ) +
    ggplot2::scale_shape_manual(
      values = c(spd_events_minus_spd = 17, events_minus_spd = 15),
      labels = contrast_labels
    ) +
    ggplot2::labs(
      x = paste(
        "Median change relative to SPD (matched interquartile range)",
        "Negative: shift toward climate | Positive: shift toward human",
        sep = "\n"
      ),
      y = NULL,
      colour = "Contrast",
      shape = "Contrast"
    ) +
    ggplot2::theme_classic(base_size = text_size) +
    ggplot2::theme(legend.position = "bottom")

  patchwork::wrap_plots(
    plot_pairs,
    plot_summary,
    ncol = 1,
    heights = c(1.4, 1)
  ) +
    patchwork::plot_annotation(
      tag_levels = "A",
      title = "Regional-event sensitivity of spatial human-climate balance"
    )
}
