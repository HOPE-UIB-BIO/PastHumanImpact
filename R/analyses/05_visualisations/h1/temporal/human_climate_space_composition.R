#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#       Human-climate-space temporal composition
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#
library(here)

source(here::here("R/00_Config_file.R"))

#----------------------------------------------------------#
# 1. Load and extract fitted results -----
#----------------------------------------------------------#
store_temporal_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "analyses_h1/human_climate_only",
      "time_slice_spd",
      sep = "/"
    )
  )

store_temporal_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "analyses_h1/human_climate_only",
      "time_slice_events",
      sep = "/"
    )
  )

output_temporal_spd <-
  targets::tar_read(
    "output_temporal_spd",
    store = store_temporal_spd
  )

output_temporal_events <-
  targets::tar_read(
    "output_temporal_events",
    store = store_temporal_events
  )

store_spatial_control_spd <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/spatial_control/spd"
  )

store_spatial_control_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/spatial_control/events"
  )

data_spatial_control_zero_truncated_composition <-
  dplyr::bind_rows(
    targets::tar_read(
      table_spatial_control_zero_truncated_composition,
      store = store_spatial_control_spd
    ),
    targets::tar_read(
      table_spatial_control_zero_truncated_composition,
      store = store_spatial_control_events
    )
  )

data_spatial_control_hierarchical_contributions <-
  dplyr::bind_rows(
    targets::tar_read(
      table_spatial_control_hierarchical_contributions,
      store = store_spatial_control_spd
    ),
    targets::tar_read(
      table_spatial_control_hierarchical_contributions,
      store = store_spatial_control_events
    )
  )

data_spatial_control_status <-
  dplyr::bind_rows(
    targets::tar_read(
      table_spatial_control_status,
      store = store_spatial_control_spd
    ),
    targets::tar_read(
      table_spatial_control_status,
      store = store_spatial_control_events
    )
  )

data_spatial_control_display_contributions <-
  prepare_spatial_hvarpart_contributions(
    data_components = data_spatial_control_hierarchical_contributions,
    data_status = data_spatial_control_status
  )

data_spatial_control_unique_adjusted_r2 <-
  dplyr::bind_rows(
    targets::tar_read(
      table_spatial_control_unique_adjusted_r2,
      store = store_spatial_control_spd
    ),
    targets::tar_read(
      table_spatial_control_unique_adjusted_r2,
      store = store_spatial_control_events
    )
  )

data_importance <-
  dplyr::bind_rows(
    compute_hvarpart_importance(
      output_temporal_spd |>
        dplyr::mutate(analysis = "temporal_spd"),
      id_cols = c("analysis", "region", "age")
    ),
    compute_hvarpart_importance(
      output_temporal_events |>
        dplyr::mutate(analysis = "temporal_events"),
      id_cols = c("analysis", "region", "age")
    )
  ) |>
    dplyr::filter(
      dplyr::between(.data[["age"]], 0, 8500),
      .data[["analysis"]] != "temporal_spd" |
        .data[["age"]] >= 2000
    )

#----------------------------------------------------------#
# 2. Summarise display profiles -----
#----------------------------------------------------------#
data_temporal_zero_truncated <-
  summarise_hvarpart_importance(
    data_importance = data_importance,
    group_vars = c("analysis", "region", "age"),
    profile = "zero_truncated"
  )

data_temporal_untruncated <-
  summarise_hvarpart_importance(
    data_importance = data_importance,
    group_vars = c("analysis", "region", "age"),
    profile = "signed"
  )

data_temporal_balance <-
  compute_hvarpart_importance_balance(
    data_summary = data_temporal_zero_truncated,
    group_vars = c("analysis", "region", "age")
  )

#----------------------------------------------------------#
# 3. Build main and supplementary figures -----
#----------------------------------------------------------#
plot_human_climate_only_composition <-
  plot_hvarpart_temporal_balance(data_temporal_balance)

plot_spatially_controlled_composition <-
  plot_h1_temporal_controlled_composition(
    data_spatial_control_zero_truncated_composition
  )

plot_untruncated_temporal_contributions <-
  plot_untruncated_temporal_hvarpart(
    data_temporal_untruncated
  )

plots_spatial_control_profiles <-
  plot_h1_temporal_control_profiles(
    data_components = data_spatial_control_display_contributions,
    data_unique_adjusted_r2 = data_spatial_control_unique_adjusted_r2,
    data_status = data_spatial_control_status
  )

#----------------------------------------------------------#
# 4. Save figures and source tables -----
#----------------------------------------------------------#
dir.create(
  here::here("Outputs/Figures/H1/Temporal/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)

purrr::walk(
  .x = c("png", "pdf"),
  .f = ~ {
    extension <- .x

    ggplot2::ggsave(
      stringr::str_c(
        here::here(
          "Outputs/Figures/H1/Temporal/HVarPart/",
          stringr::str_c(
            "spd_events__human_climate_space__",
            "zero_truncated_hierarchical_composition__space_control"
          )
        ),
        ".",
        extension
      ),
      plot = plot_spatially_controlled_composition,
      width = image_width_vec[["1col"]] * 1.5,
      height = 186.75,
      units = image_units,
      bg = "white"
    )

    ggplot2::ggsave(
      filename = stringr::str_c(
        here::here(
          "Outputs/Figures/H1/Temporal/HVarPart/",
          stringr::str_c(
            "spd_events__human_climate__",
            "zero_truncated_hierarchical_composition__human_climate_only"
          )
        ),
        ".",
        .x
      ),
      plot = plot_human_climate_only_composition,
      width = image_width_vec[["2col"]],
      height = 165,
      units = image_units,
      bg = "white"
    )

    ggplot2::ggsave(
      stringr::str_c(
        here::here(
          "Outputs/Figures/H1/Temporal/HVarPart/",
          stringr::str_c(
            "spd_events__human_climate__",
            "untruncated_hierarchical_contributions__human_climate_only"
          )
        ),
        ".",
        extension
      ),
      plot = plot_untruncated_temporal_contributions,
      width = image_width_vec[["2col"]],
      height = 165,
      units = image_units,
      bg = "white"
    )

    purrr::iwalk(
      .x = plots_spatial_control_profiles,
      .f = ~ ggplot2::ggsave(
        filename = file.path(
          here::here(
            "Outputs/Figures/H1/Temporal/HVarPart"
          ),
          stringr::str_c(
            "spd_events__human_climate_space__",
            .y,
            "__space_control",
            ".",
            extension
          )
        ),
        plot = .x,
        width = image_width_vec[["3col"]],
        height = 180,
        units = image_units,
        bg = "white"
      )
    )
  }
)

readr::write_csv(
  data_temporal_zero_truncated,
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Temporal",
    "HVarPart",
    stringr::str_c(
      "spd_events__human_climate__",
      "zero_truncated_hierarchical_composition__",
      "human_climate_only.csv"
    )
  )
)

readr::write_csv(
  data_temporal_untruncated,
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Temporal",
    "HVarPart",
    stringr::str_c(
      "spd_events__human_climate__",
      "untruncated_hierarchical_contributions__",
      "human_climate_only.csv"
    )
  )
)

readr::write_csv(
  data_temporal_balance,
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Temporal",
    "HVarPart",
    "spd_events__human_climate_balance__human_climate_only.csv"
  )
)

readr::write_csv(
  data_spatial_control_zero_truncated_composition,
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Temporal",
    "HVarPart",
    stringr::str_c(
      "spd_events__human_climate_space__",
      "zero_truncated_hierarchical_composition__",
      "space_control.csv"
    )
  )
)

readr::write_csv(
  data_spatial_control_display_contributions,
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Temporal",
    "HVarPart",
    stringr::str_c(
      "spd_events__human_climate_space__",
      "untruncated_hierarchical_contributions__",
      "space_control.csv"
    )
  )
)

readr::write_csv(
  data_spatial_control_unique_adjusted_r2,
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Temporal",
    "HVarPart",
    stringr::str_c(
      "spd_events__human_climate_space__",
      "unique_adjusted_r2__space_control.csv"
    )
  )
)
