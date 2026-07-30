#' @title Plot temporal trends for one core
#' @description
#' Plot raw, interpolated, and dataset-specific fitted trajectories for all
#' available PAP and predictor variables in one pollen core, aligned vertically
#' by age.
#' @param data_raw Pre-interpolation temporal values for one dataset.
#' @param data_observed Interpolated temporal model observations for one
#' dataset.
#' @param data_predictions Dataset-specific posterior predictions for the same
#' dataset.
#' @param data_metadata One-row metadata data frame for the dataset.
#' @param climate_palette Named character vector of climate-zone colours.
#' @param predictor_palette Named character vector containing project colours
#' for `human` and `climate` predictors.
#' @return A `ggplot` object.
#' @examples
#' \dontrun{
#' plot_core_temporal_trends(
#'   data_raw = data_core_raw,
#'   data_observed = data_core_observed,
#'   data_predictions = data_core_predictions,
#'   data_metadata = data_core_metadata
#' )
#' }
plot_core_temporal_trends <- function(
  data_raw,
  data_observed,
  data_predictions,
  data_metadata,
  climate_palette = palette_ecozones,
  predictor_palette = palette_predictors
) {
  assertthat::assert_that(
    is.data.frame(data_raw),
    is.data.frame(data_observed),
    is.data.frame(data_predictions),
    is.data.frame(data_metadata),
    msg = "Core raw data, observations, predictions, and metadata are required."
  )

  required_raw_columns <-
    c(
      "dataset_id",
      "variable",
      "variable_label",
      "climatezone",
      "age",
      "value"
    )
  required_observed_columns <-
    c(
      "dataset_id",
      "variable",
      "variable_label",
      "region",
      "climatezone",
      "age",
      "value"
    )
  required_prediction_columns <-
    c(
      "dataset_id",
      "variable",
      "variable_label",
      "climatezone",
      "age",
      "estimate",
      "conf_low",
      "conf_high"
    )

  assertthat::assert_that(
    all(required_raw_columns %in% names(data_raw)),
    all(required_observed_columns %in% names(data_observed)),
    all(required_prediction_columns %in% names(data_predictions)),
    msg = "Core plotting data are missing required columns."
  )
  assertthat::assert_that(
    nrow(data_raw) > 0L,
    nrow(data_observed) > 0L,
    nrow(data_predictions) > 0L,
    dplyr::n_distinct(data_raw[["dataset_id"]]) == 1L,
    dplyr::n_distinct(data_observed[["dataset_id"]]) == 1L,
    dplyr::n_distinct(data_predictions[["dataset_id"]]) == 1L,
    identical(
      as.character(unique(data_observed[["dataset_id"]])),
      as.character(unique(data_predictions[["dataset_id"]]))
    ),
    identical(
      as.character(unique(data_raw[["dataset_id"]])),
      as.character(unique(data_observed[["dataset_id"]]))
    ),
    msg = "Plotting data must describe the same single dataset."
  )
  assertthat::assert_that(
    is.character(climate_palette),
    !is.null(names(climate_palette)),
    is.character(predictor_palette),
    all(c("human", "climate") %in% names(predictor_palette)),
    msg = "Project palettes must contain named climate and predictor colours."
  )

  dataset_id <-
    as.character(unique(data_observed[["dataset_id"]]))
  region <-
    as.character(unique(data_observed[["region"]]))
  climatezone <-
    as.character(unique(data_observed[["climatezone"]]))
  core_handle <-
    if (
      "handle" %in% names(data_metadata) &&
        !is.na(data_metadata[["handle"]][1]) &&
        nzchar(data_metadata[["handle"]][1])
    ) {
      data_metadata[["handle"]][1]
    } else {
      dataset_id
    }

  assertthat::assert_that(
    length(region) == 1L,
    length(climatezone) == 1L,
    climatezone %in% names(climate_palette),
    msg = "Each dataset must have one region and climate zone."
  )

  figure_caption <-
    format_core_temporal_caption(data_metadata = data_metadata)
  pap_legend_label <-
    stringr::str_glue("PAP: {climatezone}")
  pap_colour <-
    unname(climate_palette[climatezone])
  subtitle_text <-
    stringr::str_glue(
      "{region}&nbsp;|&nbsp;",
      "<span style='color:{pap_colour}'><b>",
      "{climatezone}</b></span>"
    )
  figure_palette <-
    c(
      "Human" = unname(predictor_palette["human"]),
      "Climate" = unname(predictor_palette["climate"]),
      "PAP" = pap_colour
    )
  vec_figure_colours <-
    names(figure_palette)
  data_colour_groups <-
    dplyr::bind_rows(
      data_raw %>% dplyr::select(variable),
      data_observed %>% dplyr::select(variable),
      data_predictions %>% dplyr::select(variable)
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      colour_group = dplyr::case_when(
        variable == "spd" ~ "Human",
        variable %in% c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        ) ~ "Climate",
        .default = "PAP"
      ),
      colour_group = factor(
        colour_group,
        levels = vec_figure_colours
      )
    )
  data_raw_plot <-
    data_raw %>%
    dplyr::filter(
      .data[["variable"]] != "spd" | .data[["age"]] >= 2000
    ) %>%
    dplyr::left_join(
      data_colour_groups,
      by = "variable"
    )
  data_observed_plot <-
    data_observed %>%
    dplyr::filter(
      .data[["variable"]] != "spd" | .data[["age"]] >= 2000
    ) %>%
    dplyr::left_join(
      data_colour_groups,
      by = "variable"
    )
  data_predictions_plot <-
    data_predictions %>%
    dplyr::filter(
      .data[["variable"]] != "spd" | .data[["age"]] >= 2000
    ) %>%
    dplyr::left_join(
      data_colour_groups,
      by = "variable"
    )

  res_plot <-
    ggplot2::ggplot() +
    ggplot2::facet_grid(
      cols = ggplot2::vars(variable_label),
      scales = "free_x",
      space = "fixed",
      drop = FALSE,
      labeller = ggplot2::labeller(
        variable_label = ggplot2::label_wrap_gen(10)
      )
    ) +
    ggplot2::scale_x_continuous(
      breaks = scales::pretty_breaks(n = 2),
      guide = ggplot2::guide_axis(angle = 90)
    ) +
    ggplot2::scale_y_reverse(
      limits = c(8.5, 0.5),
      breaks = c(8, 6, 4, 2)
    ) +
    ggplot2::scale_colour_manual(
      values = figure_palette,
      breaks = vec_figure_colours,
      drop = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = figure_palette,
      breaks = vec_figure_colours,
      drop = FALSE
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Raw" = "dotted",
        "Interpolated" = "dashed",
        "Predicted" = "solid"
      )
    ) +
    ggplot2::labs(
      title = stringr::str_glue("dataset {dataset_id} ({core_handle})"),
      subtitle = subtitle_text,
      caption = figure_caption,
      x = NULL,
      y = "Age (cal ka BP)",
      colour = NULL,
      fill = NULL,
      linetype = NULL
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
      plot.title = ggplot2::element_text(size = 11),
      plot.subtitle = ggtext::element_markdown(size = 9),
      plot.caption = ggplot2::element_text(
        size = 7,
        hjust = 0
      ),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(size = 8)
    ) +
    ggplot2::geom_line(
      data = data_raw_plot,
      mapping = ggplot2::aes(
        x = value,
        y = age / 1000,
        colour = colour_group,
        linetype = "Raw",
        group = variable_label
      ),
      linewidth = 0.3,
      alpha = 0.45,
      orientation = "y"
    ) +
    ggplot2::geom_line(
      data = data_observed_plot,
      mapping = ggplot2::aes(
        x = value,
        y = age / 1000,
        colour = colour_group,
        linetype = "Interpolated",
        group = variable_label
      ),
      linewidth = 0.45,
      alpha = 0.75,
      orientation = "y"
    ) +
    ggplot2::geom_point(
      data = data_observed_plot,
      mapping = ggplot2::aes(
        x = value,
        y = age / 1000,
        colour = colour_group
      ),
      size = 0.45,
      alpha = 0.65
    ) +
    ggplot2::geom_ribbon(
      data = data_predictions_plot,
      mapping = ggplot2::aes(
        y = age / 1000,
        xmin = conf_low,
        xmax = conf_high,
        fill = colour_group,
        group = variable_label
      ),
      colour = NA,
      alpha = 0.18,
      orientation = "y"
    ) +
    ggplot2::geom_line(
      data = data_predictions_plot,
      mapping = ggplot2::aes(
        x = estimate,
        y = age / 1000,
        colour = colour_group,
        linetype = "Predicted",
        group = variable_label
      ),
      linewidth = 0.7,
      orientation = "y"
    )

  return(res_plot)
}
