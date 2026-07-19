#' @title Plot temporal predictor trajectories
#' @description
#' Plot observed core trajectories and modelled general trends for one temporal
#' predictor across region and climate-zone strata.
#' @param data_observed Long-format observed temporal data.
#' @param data_predictions General model predictions with uncertainty bounds.
#' @param variable Character scalar naming the variable to plot.
#' @param y_limits Numeric vector containing lower and upper response limits.
#' @param climate_palette Named character vector of climate-zone colours.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' plot_predictor_temporal_trends(
#'   data_observed = data_predictor_observed,
#'   data_predictions = data_predictor_predictions,
#'   variable = "spd",
#'   y_limits = c(0, 2.5)
#' )
#' }
plot_predictor_temporal_trends <- function(
  data_observed,
  data_predictions,
  variable,
  y_limits,
  climate_palette = palette_ecozones
) {
  required_observed_columns <-
    c(
      "region",
      "climatezone",
      "dataset_id",
      "age",
      "variable",
      "value"
    )
  required_prediction_columns <-
    c(
      "region",
      "climatezone",
      "age",
      "variable",
      "value",
      "conf_low",
      "conf_high"
    )

  assertthat::assert_that(
    is.data.frame(data_observed),
    is.data.frame(data_predictions),
    all(required_observed_columns %in% names(data_observed)),
    all(required_prediction_columns %in% names(data_predictions)),
    msg = "Predictor temporal data are missing required columns."
  )
  assertthat::assert_that(
    is.character(variable),
    length(variable) == 1L,
    !is.na(variable),
    nzchar(variable),
    variable %in% data_observed[["variable"]],
    variable %in% data_predictions[["variable"]],
    msg = "`variable` must identify observed and predicted data."
  )
  assertthat::assert_that(
    is.numeric(y_limits),
    length(y_limits) == 2L,
    all(is.finite(y_limits)),
    y_limits[[1]] < y_limits[[2]],
    is.character(climate_palette),
    !is.null(names(climate_palette)),
    msg = "Plot limits and climate palette must be valid."
  )

  data_observed_selected <-
    data_observed %>%
    dplyr::filter(.data[["variable"]] == .env$variable)
  data_predictions_selected <-
    data_predictions %>%
    dplyr::filter(.data[["variable"]] == .env$variable)
  vec_climatezones <-
    union(
      as.character(data_observed_selected[["climatezone"]]),
      as.character(data_predictions_selected[["climatezone"]])
    ) %>%
    unique()

  assertthat::assert_that(
    all(vec_climatezones %in% names(climate_palette)),
    msg = "Every plotted climate zone must have a configured colour."
  )

  res_plot <-
    ggplot2::ggplot(
      data = data_predictions_selected,
      mapping = ggplot2::aes(
        x = age / 1000,
        y = value,
        colour = climatezone,
        fill = climatezone
      )
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(region),
      cols = ggplot2::vars(climatezone),
      scales = "free_y",
      labeller = ggplot2::labeller(
        region = ggplot2::label_wrap_gen(15),
        climatezone = ggplot2::label_wrap_gen(12)
      )
    ) +
    ggplot2::scale_x_reverse(
      limits = c(8.5, 2),
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
    ggplot2::coord_cartesian(ylim = y_limits) +
    ggplot2::labs(
      x = "Age (cal ka BP)",
      y = get_temporal_variable_label(variable),
      colour = NULL,
      fill = NULL
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "none",
      panel.grid.minor = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(
        fill = "transparent",
        colour = "transparent"
      ),
      strip.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      axis.title = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      ),
      axis.text = ggplot2::element_text(
        size = text_size,
        colour = common_gray
      )
    ) +
    ggplot2::geom_line(
      data = data_observed_selected,
      mapping = ggplot2::aes(group = dataset_id),
      alpha = 0.3,
      linewidth = line_size
    ) +
    ggplot2::geom_ribbon(
      mapping = ggplot2::aes(
        ymin = conf_low,
        ymax = conf_high
      ),
      colour = NA,
      alpha = 0.3
    ) +
    ggplot2::geom_line(linewidth = 1)

  return(res_plot)
}
