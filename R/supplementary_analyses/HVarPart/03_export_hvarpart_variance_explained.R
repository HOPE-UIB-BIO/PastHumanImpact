#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#       Export HVarPart variation and correlations
#
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)
source(
  here::here("R/00_Config_file.R")
)
source(
  here::here("R/main_analysis/02_meta_data.R")
)

#----------------------------------------------------------#
# 1. Load H1 model outputs -----
#----------------------------------------------------------#

path_store_h1 <-
  stringr::str_c(
    data_storage_path,
    "Targets_data/analyses_h1"
  )

data_h1_importance <-
  dplyr::bind_rows(
    compute_hvarpart_importance(
      targets::tar_read(
        "output_spatial_spd",
        store = path_store_h1
      ) |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              .data[["dataset_id"]],
              .data[["region"]],
              .data[["climatezone"]]
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_spd"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    ),
    compute_hvarpart_importance(
      targets::tar_read(
        "output_spatial_events",
        store = path_store_h1
      ) |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              .data[["dataset_id"]],
              .data[["region"]],
              .data[["climatezone"]]
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_events"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    ),
    compute_hvarpart_importance(
      targets::tar_read(
        "output_temporal_spd",
        store = path_store_h1
      ) |>
        dplyr::filter(
          dplyr::between(
            .data[["age"]],
            2000,
            8500
          )
        ) |>
        dplyr::mutate(analysis = "temporal_spd"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    ),
    compute_hvarpart_importance(
      targets::tar_read(
        "output_temporal_events",
        store = path_store_h1
      ) |>
        dplyr::mutate(analysis = "temporal_events"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    )
  )

#----------------------------------------------------------#
# 2. Prepare variance decomposition summaries -----
#----------------------------------------------------------#

data_decomposition <-
  compute_hvarpart_variance_decomposition(
    data_importance = data_h1_importance,
    id_cols = c(
      "analysis",
      "model_id",
      "dataset_id",
      "region",
      "climatezone",
      "age"
    )
  )

table_variance_overall <-
  summarise_hvarpart_variance_decomposition(
    data_decomposition = data_decomposition,
    group_vars = "analysis"
  )
table_variance_spatial <-
  data_decomposition |>
  dplyr::filter(
    .data[["analysis"]] %in% c("spatial_spd", "spatial_events")
  ) |>
  summarise_hvarpart_variance_decomposition(
    group_vars = c("analysis", "region", "climatezone")
  )
table_variance_temporal <-
  data_decomposition |>
  dplyr::filter(
    .data[["analysis"]] %in% c("temporal_spd", "temporal_events")
  ) |>
  summarise_hvarpart_variance_decomposition(
    group_vars = c("analysis", "region", "age")
  )
table_variance_audit <-
  data_decomposition |>
  dplyr::group_by(.data[["analysis"]]) |>
  dplyr::summarise(
    n_models = dplyr::n(),
    n_available = sum(.data[["has_finite_decomposition"]]),
    n_accounting_invalid = sum(
      .data[["has_finite_decomposition"]] &
        !.data[["accounting_within_tolerance"]]
    ),
    n_negative_unique_human =
      sum(.data[["has_negative_unique_human"]], na.rm = TRUE),
    n_negative_unique_climate =
      sum(.data[["has_negative_unique_climate"]], na.rm = TRUE),
    n_negative_shared =
      sum(.data[["has_negative_shared"]], na.rm = TRUE),
    n_negative_unexplained =
      sum(.data[["has_negative_unexplained"]], na.rm = TRUE),
    max_absolute_accounting_residual = max(
      abs(.data[["accounting_residual"]][
        .data[["has_finite_decomposition"]]
      ]),
      na.rm = TRUE
    ),
    .groups = "drop"
  )

#----------------------------------------------------------#
# 3. Prepare signed correlation summaries -----
#----------------------------------------------------------#

data_correlation <-
  compute_hvarpart_correlation_values(
    data_importance = data_h1_importance |>
      dplyr::filter(.data[["analysis"]] == "spatial_spd"),
    id_cols = c(
      "analysis",
      "model_id",
      "dataset_id",
      "region",
      "climatezone"
    )
  )

table_correlation_overall <-
  summarise_hvarpart_correlations(
    data_values = data_correlation,
    importance_column = "human_importance_signed"
  )

table_correlation_grid <-
  summarise_hvarpart_correlations(
    data_values = data_correlation,
    group_vars = c(
      "region",
      "climatezone"
    ),
    importance_column = "human_importance_signed"
  )

#----------------------------------------------------------#
# 4. Prepare figures -----
#----------------------------------------------------------#

data_geo_koppen <-
  readr::read_rds(
    stringr::str_c(
      data_storage_path,
      "Spatial/Climatezones/data_geo_koppen.rds"
    )
  ) |>
  tibble::as_tibble() |>
  dplyr::mutate(
    climatezone = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 %in% c("Cold", "Temperate") ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) |>
  prepare_climatezone_factor()

plot_distribution <-
  plot_hvarpart_adjr2_distribution(
    data_decomposition = data_decomposition,
    data_meta = data_meta,
    data_geo_koppen = data_geo_koppen
  )

x_limits <-
  range(
    data_correlation[["adjusted_r_squared"]],
    finite = TRUE
  )

signed_limits <-
  range(
    data_correlation[["human_importance_signed"]],
    finite = TRUE
  )

signed_padding <-
  max(
    diff(signed_limits) * 0.04,
    0.02
  )

signed_limits <-
  signed_limits +
  c(
    -signed_padding,
    signed_padding
  )

plot_overall_signed <-
  plot_hvarpart_adjr2_importance_overall(
    data_values = data_correlation,
    data_statistics = table_correlation_overall,
    importance_column = "human_importance_signed",
    x_limits = x_limits,
    y_limits = signed_limits
  )

plot_grid_signed <-
  plot_hvarpart_adjr2_importance_grid(
    data_values = data_correlation,
    data_statistics = table_correlation_grid,
    importance_column = "human_importance_signed",
    x_limits = x_limits,
    y_limits = signed_limits
  )

#----------------------------------------------------------#
# 5. Export source tables -----
#----------------------------------------------------------#

path_output_tables <-
  here::here("Outputs/Tables/HVarPart")

path_output_figures <-
  here::here(
    "Outputs/Figures/Diagnostics/HVarPart"
  )

dir.create(
  path_output_tables,
  recursive = TRUE,
  showWarnings = FALSE
)
dir.create(
  path_output_figures,
  recursive = TRUE,
  showWarnings = FALSE
)

readr::write_csv(
  data_decomposition,
  file.path(
    path_output_tables,
    "hvarpart_variance_decomposition_models.csv"
  )
)
readr::write_csv(
  table_variance_overall,
  file.path(
    path_output_tables,
    "hvarpart_variance_summary_overall.csv"
  )
)
readr::write_csv(
  table_variance_spatial,
  file.path(
    path_output_tables,
    "hvarpart_variance_summary_spatial.csv"
  )
)
readr::write_csv(
  table_variance_temporal,
  file.path(
    path_output_tables,
    "hvarpart_variance_summary_temporal.csv"
  )
)
readr::write_csv(
  table_variance_audit,
  file.path(
    path_output_tables,
    "hvarpart_variance_missing_negative_audit.csv"
  )
)
readr::write_csv(
  plot_distribution[["record_values"]],
  file.path(
    path_output_tables,
    "HVarPart_spatial_adjr2_distribution_records.csv"
  )
)
readr::write_csv(
  plot_distribution[["climatezone_values"]],
  file.path(
    path_output_tables,
    "HVarPart_spatial_adjr2_distribution_climatezones.csv"
  )
)
readr::write_csv(
  plot_distribution[["continent_values"]],
  file.path(
    path_output_tables,
    "HVarPart_spatial_adjr2_distribution_continents.csv"
  )
)
readr::write_csv(
  data_correlation |> prepare_climatezone_factor(),
  file.path(
    path_output_tables,
    "HVarPart_adjr2_human_importance_model_values.csv"
  )
)
readr::write_csv(
  table_correlation_overall,
  file.path(
    path_output_tables,
    "HVarPart_adjr2_human_importance_overall_statistics.csv"
  )
)
readr::write_csv(
  table_correlation_grid,
  file.path(
    path_output_tables,
    "HVarPart_adjr2_human_importance_grid_statistics.csv"
  )
)

#----------------------------------------------------------#
# 6. Export figures -----
#----------------------------------------------------------#

purrr::walk(
  c(
    "png",
    "pdf"
  ),
  ~ {
    ggplot2::ggsave(
      file.path(
        path_output_figures,
        stringr::str_c(
          "HVarPart_spatial_adjr2_distribution.",
          .x
        )
      ),
      plot = plot_distribution[["plot"]],
      width = image_width_vec[["3col"]],
      height = 180,
      units = image_units,
      bg = "white"
    )
    ggplot2::ggsave(
      file.path(
        path_output_figures,
        stringr::str_c(
          "HVarPart_adjr2_human_importance_overall.",
          .x
        )
      ),
      plot = plot_overall_signed,
      width = image_width_vec[["2col"]],
      height = 120,
      units = image_units,
      bg = "white"
    )
    ggplot2::ggsave(
      file.path(
        path_output_figures,
        stringr::str_c(
          "HVarPart_adjr2_human_importance_grid.",
          .x
        )
      ),
      plot = plot_grid_signed,
      width = image_width_vec[["3col"]],
      height = 200,
      units = image_units,
      bg = "white"
    )
  }
)
