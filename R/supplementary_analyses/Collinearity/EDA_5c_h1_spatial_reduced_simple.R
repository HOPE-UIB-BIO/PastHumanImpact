#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#       Extended data analysis - PAP collinearity
#   Part C: Simple Figure 2 style chart (reduced PAP set)
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

source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)

#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

output_spatial_spd_reduced <-
  targets::tar_read(
    name = "output_spatial_spd_reduced_collinear_v1",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "analyses_h1_reviewer_collinearity"
    )
  )

#----------------------------------------------------------#
# 2. Prepare summaries -----
#----------------------------------------------------------#

data_spatial_reduced <-
  output_spatial_spd_reduced |>
  dplyr::left_join(
    data_meta |>
      dplyr::select(
        dataset_id,
        region,
        climatezone
      ),
    by = "dataset_id"
  ) |>
  dplyr::mutate(analysis = "spatial_spd_reduced") |>
  get_hvarpart_importance(
    data_source = _,
    id_cols = c("analysis", "dataset_id", "region", "climatezone")
  ) |>
  summarise_hvarpart_importance(
    group_vars = c("analysis", "region", "climatezone"),
    profile = "signed"
  ) |>
  dplyr::mutate(
    ratio = pooled_allocation,
    predictor = dplyr::case_when(
      predictor == "human" ~ "Humans",
      predictor == "climate" ~ "Climate",
      .default = predictor
    ),
    predictor = factor(
      predictor,
      levels = c("Climate", "Humans")
    )
  ) |>
  add_region_as_factor() |>
  add_climatezone_as_factor() |>
  dplyr::mutate(
    climatezone_label = factor(
      climatezone_label,
      levels = get_climatezone_label(data_climate_zones$climatezone_label)
    )
  ) |>
  dplyr::filter(
    !is.na(region),
    !is.na(climatezone_label),
    !is.na(predictor)
  )

#----------------------------------------------------------#
# 3. Plot -----
#----------------------------------------------------------#

plot_reduced_simple <-
  data_spatial_reduced |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = climatezone_label,
      y = ratio,
      fill = predictor
    )
  ) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.9),
    linewidth = line_size * 0.5,
    color = "white"
  ) +
  ggplot2::facet_wrap(
    ~ region,
    ncol = 1,
    scales = "fixed"
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Humans" = palette_predictors[["human"]],
      "Climate" = palette_predictors[["climate"]]
    )
  ) +
  ggplot2::geom_hline(yintercept = c(0, 1), colour = "grey70") +
  ggplot2::scale_y_continuous() +
  ggplot2::scale_x_discrete(
    drop = FALSE
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
    axis.title.x = ggplot2::element_blank(),
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    ),
    legend.position = "bottom",
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    y = "Ratio of importance",
    fill = "Predictors"
  )

#----------------------------------------------------------#
# 4. Save outputs -----
#----------------------------------------------------------#

readr::write_csv(
  data_spatial_reduced,
  here::here(
    "Outputs/Tables/Collinearity/pap_collinearity_reduced_simple_fig2_table.csv"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    filename = stringr::str_c(
      here::here(
        "Outputs/Figures/Extended_data_figures/Collinearity/EDA_5c_h1_spatial_reduced_simple"
      ),
      ".",
      .x
    ),
    plot = plot_reduced_simple,
    width = image_width_vec[["2col"]],
    height = 160,
    units = image_units,
    bg = "transparent"
  )
)
