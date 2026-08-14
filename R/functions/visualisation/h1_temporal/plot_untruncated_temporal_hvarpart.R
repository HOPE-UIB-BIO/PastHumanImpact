#' @title Plot signed temporal HVarPart allocations
#' @description
#' Uses unstacked bars and explicit zero/one reference lines so negative and
#' greater-than-one allocations remain interpretable while retaining the
#' original age-panel structure.
#' @param data_summary Signed pooled temporal summary data.
#' @return A ggplot object.
#' @examples
#' \dontrun{
#' plot_untruncated_temporal_hvarpart(temporal_signed_summary)
#' }
plot_untruncated_temporal_hvarpart <- function(data_summary) {
  required <- c("analysis", "region", "age", "predictor", "pooled_allocation")
  assertthat::assert_that(
    is.data.frame(data_summary),
    all(required %in% names(data_summary)),
    msg = "Temporal HVarPart summary columns are missing."
  )

  series_palette <-
    c(
      "Climate (SPD)" = palette_predictors[["climate"]],
      "Climate (Events)" =
        colorspace::lighten(palette_predictors[["climate"]], amount = 0.5),
      "Humans (SPD)" = palette_predictors[["human"]],
      "Humans (Events)" =
        colorspace::lighten(palette_predictors[["human"]], amount = 0.5)
    )
  signed_region_labeller <- region_labeller
  signed_region_labeller[["Latin America"]] <-
    "Central &\nSouth America"

  data_plot <-
    data_summary |>
    dplyr::filter(
      dplyr::between(.data[["age"]], 0, 8500),
      .data[["analysis"]] != "temporal_spd" | .data[["age"]] >= 2000
    ) |>
    dplyr::mutate(
      human_predictor = dplyr::recode(
        .data[["analysis"]],
        temporal_spd = "SPD",
        temporal_events = "Events"
      ),
      human_predictor = factor(
        .data[["human_predictor"]],
        levels = c("SPD", "Events")
      ),
      predictor = factor(
        .data[["predictor"]],
        levels = c("climate", "human")
      ),
      predictor_proxy = dplyr::case_when(
        .data[["predictor"]] == "climate" &
          .data[["human_predictor"]] == "SPD" ~ "Climate (SPD)",
        .data[["predictor"]] == "climate" ~ "Climate (Events)",
        .data[["human_predictor"]] == "SPD" ~ "Humans (SPD)",
        .default = "Humans (Events)"
      ),
      predictor_proxy = factor(
        .data[["predictor_proxy"]],
        levels = names(series_palette)
      ),
      age_ka = .data[["age"]] / 1000,
      age_label = factor(
        scales::number(
          .data[["age_ka"]],
          accuracy = 0.1,
          trim = TRUE
        ),
        levels = scales::number(
          seq(8.5, 0.5, -0.5),
          accuracy = 0.1,
          trim = TRUE
        )
      ),
      region = factor(
        .data[["region"]],
        levels = c(
          "North America", "Latin America", "Europe", "Asia", "Oceania"
        )
      )
    )

  result <-
    ggplot2::ggplot(
      data_plot,
      ggplot2::aes(
        x = .data[["predictor"]],
        y = .data[["pooled_allocation"]],
        fill = .data[["predictor_proxy"]]
      )
    ) +
    ggplot2::geom_hline(
      yintercept = 1,
      colour = "grey70"
    ) +
    ggplot2::geom_hline(
      yintercept = 0,
      colour = "grey70",
      linetype = 2
    ) +
    ggplot2::geom_col(
      width = 0.7,
      position = ggplot2::position_dodge(width = 0.75),
      colour = common_gray,
      linewidth = line_size,
      show.legend = TRUE
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data[["region"]]),
      cols = ggplot2::vars(.data[["age_label"]]),
      switch = "both",
      drop = FALSE,
      labeller = ggplot2::labeller(
        region = ggplot2::as_labeller(signed_region_labeller)
      )
    ) +
    ggplot2::scale_x_discrete(
      drop = FALSE,
      expand = ggplot2::expansion(mult = 0.08)
    ) +
    ggplot2::scale_y_continuous(position = "right") +
    ggplot2::scale_fill_manual(
      "Predictor and proxy",
      values = series_palette,
      drop = FALSE
    ) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(
        title.position = "top",
        nrow = 2,
        ncol = 2,
        byrow = TRUE
      )
    ) +
    ggplot2::labs(
      x = "Age (ka BP)",
      y = "Signed allocation of adjusted explained variation",
      fill = NULL
    ) +
    ggplot2::theme_bw(base_size = text_size) +
    ggplot2::theme(
      plot.margin = ggplot2::margin(
        t = 3,
        r = 3,
        b = 3,
        l = 8,
        unit = "mm"
      ),
      legend.position = "bottom",
      panel.spacing.x = ggplot2::unit(0.5, "mm"),
      panel.spacing.y = ggplot2::unit(3, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text.y.left = ggplot2::element_text(
        angle = 90,
        margin = ggplot2::margin(l = 2, r = 2, unit = "mm")
      ),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank()
    )

  return(result)
}
