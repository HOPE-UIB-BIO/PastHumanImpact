#' @title Plot modelled PAP trajectories
#' @description
#' Plot modelled region-climate PAP trajectories. The primary layout shows
#' general trends only, while the stratum layout also shows observed core
#' trajectories.
#' @param data_observed PAP observations with region and climate-zone metadata.
#' @param data_predictions Modelled PAP trajectories and credible intervals.
#' @param layout Character scalar, either `"primary"` or `"strata"`.
#' @param climate_palette Named character vector of climate-zone colours.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' plot_pap_temporal_trends(
#'   data_observed = data_pap_observed,
#'   data_predictions = data_pap_predictions,
#'   layout = "primary"
#' )
#' }
plot_pap_temporal_trends <- function(
  data_observed,
  data_predictions,
  layout = c("primary", "strata"),
  climate_palette = palette_ecozones
) {
  assertthat::assert_that(
    is.data.frame(data_observed),
    is.data.frame(data_predictions),
    msg = "PAP observations and predictions must be data frames."
  )

  required_observed_columns <-
    c(
      "variable",
      "pap_label",
      "region",
      "climatezone",
      "climatezone_label",
      "dataset_id",
      "age",
      "value"
    )
  required_prediction_columns <-
    c(
      "variable",
      "pap_label",
      "region",
      "climatezone",
      "climatezone_label",
      "age",
      "estimate",
      "conf_low",
      "conf_high"
    )

  assertthat::assert_that(
    all(required_observed_columns %in% names(data_observed)),
    all(required_prediction_columns %in% names(data_predictions)),
    msg = "PAP plotting data are missing required columns."
  )
  assertthat::assert_that(
    is.character(climate_palette),
    !is.null(names(climate_palette)),
    msg = "`climate_palette` must be a named character vector."
  )

  layout <-
    match.arg(layout)

  res_plot <-
    ggplot2::ggplot() +
    ggplot2::scale_x_reverse(
      limits = c(8.5, 0.5),
      breaks = c(8, 6, 4, 2)
    ) +
    ggplot2::scale_colour_manual(
      values = climate_palette,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = climate_palette,
      drop = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "white",
        colour = "grey70"
      ),
      strip.text = ggplot2::element_text(size = 8),
      axis.title = ggplot2::element_text(size = 9),
      axis.text = ggplot2::element_text(size = 7),
      legend.text = ggplot2::element_text(size = 7),
      legend.title = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      x = "Age (cal ka BP)",
      y = NULL,
      colour = NULL,
      fill = NULL
    )

  if (
    layout == "primary"
  ) {
    res_plot <-
      res_plot +
      ggplot2::facet_grid(
        rows = ggplot2::vars(pap_label),
        cols = ggplot2::vars(region),
        scales = "free_y",
        switch = "y"
      ) +
      ggplot2::theme(
        legend.position = "bottom",
        strip.placement = "outside",
        strip.text.y.left = ggplot2::element_text(
          angle = 0,
          hjust = 1
        )
      ) +
      ggplot2::guides(
        colour = ggplot2::guide_legend(nrow = 2),
        fill = ggplot2::guide_legend(nrow = 2)
      )
  } else {
    res_plot <-
      res_plot +
      ggplot2::facet_grid(
        rows = ggplot2::vars(region),
        cols = ggplot2::vars(climatezone),
        scales = "free_y",
        labeller = ggplot2::labeller(
          region = ggplot2::label_wrap_gen(15),
          climatezone = ggplot2::label_wrap_gen(12)
        )
      ) +
      ggplot2::theme(legend.position = "none") +
      ggplot2::geom_line(
        data = data_observed,
        mapping = ggplot2::aes(
          x = age / 1000,
          y = value,
          group = dataset_id,
          colour = climatezone
        ),
        linewidth = 0.15,
        alpha = 0.12
      )
  }

  res_plot <-
    res_plot +
    ggplot2::geom_ribbon(
      data = data_predictions,
      mapping = ggplot2::aes(
        x = age / 1000,
        ymin = conf_low,
        ymax = conf_high,
        fill = climatezone,
        group = climatezone
      ),
      colour = NA,
      alpha = 0.18
    ) +
    ggplot2::geom_line(
      data = data_predictions,
      mapping = ggplot2::aes(
        x = age / 1000,
        y = estimate,
        colour = climatezone,
        group = climatezone
      ),
      linewidth = 0.55
    )

  return(res_plot)
}
