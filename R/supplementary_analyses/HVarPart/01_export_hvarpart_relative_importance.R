library(here)
source(here::here("R/00_Config_file.R"))
source(here::here("R/main_analysis/02_meta_data.R"))

store_h1 <- paste0(data_storage_path, "Targets_data/analyses_h1")
store_h2 <- paste0(data_storage_path, "Targets_data/analyses_h2")

data_h1_importance <- dplyr::bind_rows(
  get_hvarpart_importance(
    targets::tar_read("output_spatial_spd", store = store_h1) |>
      dplyr::left_join(
        data_meta |>
          dplyr::select(dataset_id, region, climatezone),
        by = "dataset_id"
      ) |>
      dplyr::mutate(analysis = "spatial_spd"),
    id_cols = c("analysis", "dataset_id", "region", "climatezone")
  ),
  get_hvarpart_importance(
    targets::tar_read("output_spatial_events", store = store_h1) |>
      dplyr::left_join(
        data_meta |>
          dplyr::select(dataset_id, region, climatezone),
        by = "dataset_id"
      ) |>
      dplyr::mutate(analysis = "spatial_events"),
    id_cols = c("analysis", "dataset_id", "region", "climatezone")
  ),
  get_hvarpart_importance(
    targets::tar_read("output_temporal_spd", store = store_h1) |>
      dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500)) |>
      dplyr::mutate(analysis = "temporal_spd"),
    id_cols = c("analysis", "region", "age")
  ),
  get_hvarpart_importance(
    targets::tar_read("output_temporal_events", store = store_h1) |>
      dplyr::mutate(analysis = "temporal_events"),
    id_cols = c("analysis", "region", "age")
  )
)

data_h2_importance <- get_hvarpart_importance(
  targets::tar_read("output_hvar_h2_spd", store = store_h2) |>
    dplyr::mutate(analysis = "h2_spd"),
  id_cols = c("analysis", "region", "climatezone")
)

analysis_export_order <- c(
  "temporal_spd",
  "temporal_events",
  "spatial_spd",
  "spatial_events",
  "h2_spd"
)
data_importance <-
  dplyr::bind_rows(data_h1_importance, data_h2_importance) |>
  dplyr::arrange(
    match(.data[["analysis"]], analysis_export_order),
    .data[["model_id"]],
    .data[["predictor"]]
  )

table_audit <- dplyr::bind_rows(
  summarise_hvarpart_audit(data_importance, "analysis") |>
    dplyr::mutate(aggregation_level = "analysis"),
  summarise_hvarpart_audit(
    data_h1_importance |>
      dplyr::filter(stringr::str_starts(.data[["analysis"]], "spatial")),
    c("analysis", "region", "climatezone")
  ) |>
    dplyr::mutate(aggregation_level = "region_climatezone"),
  summarise_hvarpart_audit(
    data_h1_importance |>
      dplyr::filter(stringr::str_starts(.data[["analysis"]], "temporal")),
    c("analysis", "region", "age")
  ) |>
    dplyr::mutate(aggregation_level = "region_age"),
  summarise_hvarpart_audit(
    data_h2_importance,
    c("analysis", "region", "climatezone")
  ) |>
    dplyr::mutate(aggregation_level = "region_climatezone")
) |>
  dplyr::relocate(
    dplyr::all_of("aggregation_level"),
    .after = dplyr::all_of("analysis")
  )

table_profiles <- dplyr::bind_rows(
  compare_hvarpart_importance_profiles(data_importance, "analysis") |>
    dplyr::mutate(aggregation_level = "analysis"),
  compare_hvarpart_importance_profiles(
    data_importance,
    c("analysis", "model_id")
  ) |>
    dplyr::mutate(aggregation_level = "model"),
  compare_hvarpart_importance_profiles(
    data_h1_importance |>
      dplyr::filter(stringr::str_starts(.data[["analysis"]], "spatial")),
    c("analysis", "region", "climatezone")
  ) |>
    dplyr::mutate(aggregation_level = "region_climatezone"),
  compare_hvarpart_importance_profiles(
    data_h1_importance |>
      dplyr::filter(stringr::str_starts(.data[["analysis"]], "temporal")),
    c("analysis", "region", "age")
  ) |>
    dplyr::mutate(aggregation_level = "region_age"),
  compare_hvarpart_importance_profiles(
    data_h2_importance,
    c("analysis", "region", "climatezone")
  ) |>
    dplyr::mutate(aggregation_level = "region_climatezone")
) |>
  dplyr::relocate(
    dplyr::all_of("aggregation_level"),
    .after = dplyr::all_of("analysis")
  )

output_table_dir <- here::here("Outputs/Tables/HVarPart")
output_figure_dir <- here::here("Outputs/Figures/Extended_data_figures/HVarPart")
dir.create(output_table_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_figure_dir, recursive = TRUE, showWarnings = FALSE)

readr::write_csv(
  data_importance,
  file.path(output_table_dir, "hvarpart_components.csv")
)
readr::write_csv(
  table_audit,
  file.path(output_table_dir, "hvarpart_model_audit.csv")
)
readr::write_csv(
  table_profiles,
  file.path(output_table_dir, "hvarpart_profile_comparison.csv")
)

figure_profiles <-
  plot_hvarpart_profile_comparison(table_profiles)

purrr::walk(c("png", "pdf"), function(extension) {
  ggplot2::ggsave(
    file.path(
      output_figure_dir,
      paste0("hvarpart_profile_comparison.", extension)
    ),
    plot = figure_profiles,
    width = image_width_vec[["2col"]],
    height = 110,
    units = image_units,
    bg = "white"
  )
})
