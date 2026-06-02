#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#       Extended data analysis - PAP collinearity
#         Part A: Correlation among PAP variables
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_collinearity <-
  targets::tar_read(
    name = "output_pap_collinearity_spatial",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "analyses_h1_reviewer_collinearity"
    )
  )

#----------------------------------------------------------#
# 2. Prepare summaries -----
#----------------------------------------------------------#

data_correlation <-
  data_collinearity |>
  purrr::pluck("correlation_table")

data_correlation_summary <-
  data_correlation |>
  dplyr::filter(
    is_eligible,
    !is.na(x),
    !is.na(y)
  ) |>
  dplyr::mutate(
    pair = stringr::str_c(
      pmin(x, y),
      pmax(x, y),
      sep = "__"
    )
  ) |>
  dplyr::group_by(pair) |>
  dplyr::summarise(
    n_groups = dplyr::n(),
    median_abs_correlation = stats::median(abs_correlation, na.rm = TRUE),
    q95_abs_correlation = stats::quantile(abs_correlation, 0.95, na.rm = TRUE),
    .groups = "drop"
  ) |>
  tidyr::separate(
    col = pair,
    into = c("x", "y"),
    sep = "__"
  )

#----------------------------------------------------------#
# 3. Plot -----
#----------------------------------------------------------#

plot_correlation <-
  data_correlation_summary |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = x,
      y = y,
      fill = median_abs_correlation
    )
  ) +
  ggplot2::geom_tile() +
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.2f", median_abs_correlation)
    ),
    size = 2
  ) +
  ggplot2::scale_fill_viridis_c(
    limits = c(0, 1),
    option = "C"
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      angle = 45,
      hjust = 1,
      size = text_size
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size
    ),
    axis.title = ggplot2::element_blank(),
    legend.position = "bottom",
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    fill = "Median |r|"
  )

#----------------------------------------------------------#
# 4. Save outputs -----
#----------------------------------------------------------#

readr::write_csv(
  data_correlation_summary,
  here::here(
    "Outputs/Tables/pap_collinearity_summary.csv"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    filename = stringr::str_c(
      here::here(
        "Outputs/Figures/Extended_data_figures/EDA_5a_pap_collinearity_correlation"
      ),
      ".",
      .x
    ),
    plot = plot_correlation,
    width = image_width_vec[["2col"]],
    height = image_width_vec[["2col"]],
    units = image_units,
    bg = "transparent"
  )
)
