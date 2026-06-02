#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#       Extended data analysis - PAP collinearity
#       Part B: Difference in explained variance ratio
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

data_hvar_spatial_spd_baseline <-
  targets::tar_read(
    name = "output_spatial_spd",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "analyses_h1_reviewer_collinearity"
    )
  )

data_hvar_spatial_spd_reduced <-
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

data_hvar_importance <-
  dplyr::bind_rows(
    extract_hvar_importance(
      data_hvar = data_hvar_spatial_spd_baseline,
      model_label = "baseline"
    ),
    extract_hvar_importance(
      data_hvar = data_hvar_spatial_spd_reduced,
      model_label = "reduced_collinear_v1"
    )
  ) |>
  dplyr::arrange(dataset_id, predictor, model)

data_hvar_influence <-
  data_hvar_importance |>
  dplyr::select(
    dataset_id,
    predictor,
    model,
    importance_ratio
  ) |>
  tidyr::pivot_wider(
    names_from = model,
    values_from = importance_ratio
  ) |>
  dplyr::mutate(
    delta_ratio = reduced_collinear_v1 - baseline,
    abs_delta_ratio = abs(delta_ratio)
  )

data_hvar_influence_summary <-
  data_hvar_influence |>
  dplyr::group_by(predictor) |>
  dplyr::summarise(
    n_datasets = dplyr::n(),
    n_with_complete_pairs = sum(
      !is.na(baseline) & !is.na(reduced_collinear_v1)
    ),
    median_baseline = stats::median(baseline, na.rm = TRUE),
    median_reduced = stats::median(reduced_collinear_v1, na.rm = TRUE),
    median_delta_ratio = stats::median(delta_ratio, na.rm = TRUE),
    q95_abs_delta_ratio = stats::quantile(
      abs_delta_ratio,
      0.95,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#----------------------------------------------------------#
# 3. Plot -----
#----------------------------------------------------------#

plot_hvar_influence <-
  data_hvar_influence |>
  dplyr::filter(
    !is.na(delta_ratio)
  ) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = predictor,
      y = delta_ratio,
      fill = predictor
    )
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = common_gray,
    linewidth = line_size
  ) +
  ggplot2::geom_violin(
    alpha = 0.5,
    linewidth = line_size
  ) +
  ggplot2::geom_boxplot(
    width = 0.2,
    outlier.alpha = 0.2,
    linewidth = line_size
  ) +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(
      size = text_size
    ),
    axis.text.y = ggplot2::element_text(
      size = text_size
    ),
    axis.title.x = ggplot2::element_blank(),
    legend.position = "none",
    text = ggplot2::element_text(
      size = text_size,
      color = common_gray
    )
  ) +
  ggplot2::labs(
    y = "Delta Importance Ratio (Reduced - Baseline)"
  )

#----------------------------------------------------------#
# 4. Save outputs -----
#----------------------------------------------------------#

readr::write_csv(
  data_hvar_influence,
  here::here(
    "Outputs/Tables/Collinearity/pap_collinearity_hvar_influence_by_dataset.csv"
  )
)

readr::write_csv(
  data_hvar_influence_summary,
  here::here(
    "Outputs/Tables/Collinearity/pap_collinearity_hvar_influence_summary.csv"
  )
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    filename = stringr::str_c(
      here::here(
        "Outputs/Figures/Extended_data_figures/Collinearity/EDA_5b_pap_collinearity_hvar_influence"
      ),
      ".",
      .x
    ),
    plot = plot_hvar_influence,
    width = image_width_vec[["2col"]],
    height = image_width_vec[["1col"]],
    units = image_units,
    bg = "transparent"
  )
)
