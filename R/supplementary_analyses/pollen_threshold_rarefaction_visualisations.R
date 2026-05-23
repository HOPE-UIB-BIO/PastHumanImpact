#----------------------------------------------------------#
#
#                PastHumanImpact Diagnostics
#
#      Visualise selected pollen threshold summaries
#
#----------------------------------------------------------#

library(here)

source(
  here::here("R/00_Config_file.R")
)

#----------------------------------------------------------#
# 1. Configuration -----
#----------------------------------------------------------#

path_tables <-
  here::here("Outputs", "Tables")

path_figures <-
  here::here("Outputs", "Figures", "Supplementary_analyses")

if (
  isFALSE(dir.exists(path_figures))
) {
  dir.create(path_figures, recursive = TRUE)
}

palette_ecozones_labels <-
  palette_ecozones |>
  rlang::set_names(
    nm = get_climatezone_label(names(palette_ecozones))
  )

format_threshold_axis <- function(x) {
  ifelse(x == 0, "(no filtering)", as.character(x))
}

#----------------------------------------------------------#
# 2. Load data -----
#----------------------------------------------------------#

data_retention_overall <-
  readr::read_csv(
    file.path(path_tables, "pollen_threshold_retention_overall.csv"),
    show_col_types = FALSE
  )

vec_thresholds_plot <-
  c(0, sort(unique(data_retention_overall$threshold)))

data_retention_by_region <-
  readr::read_csv(
    file.path(path_tables, "pollen_threshold_retention_by_region.csv"),
    show_col_types = FALSE
  ) |>
  add_region_as_factor()

data_retention_by_region_climatezone <-
  readr::read_csv(
    file.path(path_tables, "pollen_threshold_retention_by_region_climatezone.csv"),
    show_col_types = FALSE
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  dplyr::filter(
    is.na(climatezone) == FALSE,
    is.na(climatezone_label) == FALSE
  )

data_sample_rowsums_climatezone <-
  readr::read_csv(
    file.path(path_tables, "pollen_sample_rowsums_by_climatezone.csv"),
    show_col_types = FALSE
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  dplyr::filter(
    is.na(region) == FALSE,
    is.na(climatezone) == FALSE,
    is.na(rowsum) == FALSE
  )

#----------------------------------------------------------#
# 3. Prepare plot data -----
#----------------------------------------------------------#

data_retention_overall_plot <-
  data_retention_overall |>
  dplyr::select(threshold, prop_samples_retained, prop_retained) |>
  tidyr::pivot_longer(
    cols = c(prop_samples_retained, prop_retained),
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = factor(
      metric,
      levels = c("prop_samples_retained", "prop_retained"),
      labels = c("Samples retained", "Datasets retained")
    )
  ) |>
  dplyr::bind_rows(
    tibble::tibble(
      threshold = 0,
      metric = factor(
        c("Samples retained", "Datasets retained"),
        levels = c("Samples retained", "Datasets retained")
      ),
      value = 1
    )
  ) |>
  dplyr::distinct(threshold, metric, .keep_all = TRUE) |>
  dplyr::arrange(metric, threshold)

data_retention_region_plot <-
  data_retention_by_region |>
  dplyr::select(region, threshold, prop_region_samples_retained, prop_region_retained) |>
  tidyr::pivot_longer(
    cols = c(prop_region_samples_retained, prop_region_retained),
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = factor(
      metric,
      levels = c("prop_region_samples_retained", "prop_region_retained"),
      labels = c("Samples retained", "Datasets retained")
    )
  ) |>
  dplyr::bind_rows(
    data_retention_by_region |>
      dplyr::distinct(region) |>
      tidyr::crossing(
        metric = factor(
          c("Samples retained", "Datasets retained"),
          levels = c("Samples retained", "Datasets retained")
        )
      ) |>
      dplyr::mutate(
        threshold = 0,
        value = 1
      )
  ) |>
  dplyr::filter(
    is.na(region) == FALSE
  ) |>
  dplyr::mutate(
    region_facet = dplyr::case_when(
      region == "Latin America" ~ "Central & South America",
      TRUE ~ as.character(region)
    ),
    region_facet = factor(
      region_facet,
      levels = c(
        "North America",
        "Europe",
        "Asia",
        "Central & South America",
        " ",
        "Oceania"
      )
    )
  ) |>
  dplyr::distinct(region, threshold, metric, .keep_all = TRUE) |>
  dplyr::arrange(region, metric, threshold)

data_retention_region_climatezone_plot <-
  data_retention_by_region_climatezone |>
  dplyr::select(
    region,
    climatezone_label,
    threshold,
    prop_region_climatezone_samples_retained,
    prop_region_climatezone_retained
  ) |>
  tidyr::pivot_longer(
    cols = c(
      prop_region_climatezone_samples_retained,
      prop_region_climatezone_retained
    ),
    names_to = "metric",
    values_to = "value"
  ) |>
  dplyr::mutate(
    metric = factor(
      metric,
      levels = c(
        "prop_region_climatezone_samples_retained",
        "prop_region_climatezone_retained"
      ),
      labels = c("Samples retained", "Datasets retained")
    )
  ) |>
  dplyr::bind_rows(
    data_retention_by_region_climatezone |>
      dplyr::distinct(region, climatezone_label) |>
      tidyr::crossing(
        metric = factor(
          c("Samples retained", "Datasets retained"),
          levels = c("Samples retained", "Datasets retained")
        )
      ) |>
      dplyr::mutate(
        threshold = 0,
        value = 1
      )
  ) |>
  tidyr::complete(
    tidyr::nesting(region, climatezone_label, metric),
    threshold = vec_thresholds_plot,
    fill = list(value = 0)
  ) |>
  dplyr::mutate(
    value = dplyr::if_else(threshold == 0, 1, value)
  ) |>
  dplyr::distinct(region, climatezone_label, threshold, metric, .keep_all = TRUE) |>
  dplyr::arrange(region, climatezone_label, metric, threshold)

#----------------------------------------------------------#
# 4. Build selected figures -----
#----------------------------------------------------------#

fig_density_rowsum_climatezone <-
  data_sample_rowsums_climatezone |>
  dplyr::filter(rowsum > 0) |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = rowsum,
      y = ggplot2::after_stat(count),
      fill = climatezone_label
    )
  ) +
  ggplot2::geom_histogram(
    bins = 60,
    position = "stack",
    alpha = 0.9,
    color = NA
  ) +
  ggplot2::facet_wrap(
    ~region,
    scales = "free_y",
    ncol = 1
  ) +
  ggplot2::scale_fill_manual(
    values = palette_ecozones_labels,
    name = "Climate zone"
  ) +
  ggplot2::scale_x_continuous(
    trans = "log10",
    breaks = c(25, 100, 150, 300, 500, 1000, 5000),
    labels = scales::label_number(big.mark = ",")
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "right",
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    x = "Pollen grains per sample",
    y = "Number of samples",
    title = "Stacked sample-count distribution by climate zone and continent"
  )

fig_retention_overall <-
  data_retention_overall_plot |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = threshold,
      y = value,
      group = metric
    )
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(data_retention_overall_plot, metric == "Samples retained"),
    linewidth = 1,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(data_retention_overall_plot, metric == "Samples retained"),
    size = 2.5,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(data_retention_overall_plot, metric == "Datasets retained"),
    linewidth = 1,
    linetype = "dashed",
    alpha = 0.5,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(data_retention_overall_plot, metric == "Datasets retained"),
    size = 2.5,
    alpha = 0.5,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(data_retention_overall_plot$threshold)),
    labels = format_threshold_axis
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    legend.position = "bottom",
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    x = "Applied grain threshold",
    y = "Proportion retained",
    title = "Overall retention under stricter thresholds",
    subtitle = "Solid = samples retained; dashed = datasets retained"
  )

fig_retention_region_facets <-
  data_retention_region_plot |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = threshold,
      y = value
    )
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(data_retention_region_plot, metric == "Samples retained"),
    mapping = ggplot2::aes(group = region),
    linewidth = 0.9,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(data_retention_region_plot, metric == "Samples retained"),
    mapping = ggplot2::aes(group = region),
    size = 2,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(data_retention_region_plot, metric == "Datasets retained"),
    mapping = ggplot2::aes(group = region),
    linewidth = 0.9,
    linetype = "dashed",
    alpha = 0.5,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(data_retention_region_plot, metric == "Datasets retained"),
    mapping = ggplot2::aes(group = region),
    size = 2,
    alpha = 0.5,
    color = unname(palette_predictors["climate"])
  ) +
  ggplot2::facet_wrap(
    ~region_facet,
    nrow = 2,
    drop = FALSE
  ) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(data_retention_region_plot$threshold)),
    labels = format_threshold_axis
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey95"),
    axis.text.x = ggplot2::element_text(
      angle = -45,
      hjust = 0,
      vjust = 1
    ),
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    x = "Applied grain threshold",
    y = "Proportion retained relative to regional baseline",
    title = "Retention by region (each relative to its own baseline)",
    subtitle = "Solid = samples retained; dashed = datasets retained"
  )

fig_retention_region_climatezone <-
  data_retention_region_climatezone_plot |>
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = threshold,
      y = value,
      color = climatezone_label,
      group = climatezone_label
    )
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(
      data_retention_region_climatezone_plot,
      metric == "Samples retained"
    ),
    linewidth = 0.8
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(
      data_retention_region_climatezone_plot,
      metric == "Samples retained"
    ),
    size = 1.6
  ) +
  ggplot2::geom_line(
    data = dplyr::filter(
      data_retention_region_climatezone_plot,
      metric == "Datasets retained"
    ),
    linewidth = 0.8,
    linetype = "dashed",
    alpha = 0.5
  ) +
  ggplot2::geom_point(
    data = dplyr::filter(
      data_retention_region_climatezone_plot,
      metric == "Datasets retained"
    ),
    size = 1.6,
    alpha = 0.5
  ) +
  ggplot2::facet_grid(climatezone_label ~ region) +
  ggplot2::scale_x_continuous(
    breaks = sort(unique(data_retention_region_climatezone_plot$threshold)),
    labels = format_threshold_axis
  ) +
  ggplot2::scale_y_continuous(
    breaks = c(0, 0.5, 1),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  ggplot2::scale_color_manual(
    values = palette_ecozones_labels
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    strip.background = ggplot2::element_rect(fill = "grey95"),
    legend.position = "none",
    axis.text.x = ggplot2::element_text(
      angle = -45,
      hjust = 0,
      vjust = 1
    ),
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    x = "Applied grain threshold",
    y = "Proportion retained relative to region-climatezone baseline",
    title = "Retention by climate zone within each region",
    subtitle = "Solid = samples retained; dashed = datasets retained"
  )

#----------------------------------------------------------#
# 5. Save selected figures -----
#----------------------------------------------------------#

fig_list <-
  list(
    supplement_density_rowsum_by_climatezone = fig_density_rowsum_climatezone,
    supplement_threshold_retention_overall = fig_retention_overall,
    supplement_threshold_retention_region_facets = fig_retention_region_facets,
    supplement_threshold_retention_region_climatezone = fig_retention_region_climatezone
  )

for (fig_name in names(fig_list)) {
  fig_object <- fig_list[[fig_name]]

  purrr::walk(
    .x = c("png", "pdf"),
    .f = ~ ggplot2::ggsave(
      filename = file.path(path_figures, paste0(fig_name, ".", .x)),
      plot = fig_object,
      width = image_width_vec["2col"],
      height = 170,
      units = image_units,
      bg = "white"
    )
  )
}

message("Selected supplementary figures exported successfully.")
