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
store_h1 <-
  paste0(data_storage_path, "Targets_data/analyses_h1")

output_temporal_spd <-
  targets::tar_read(
    "output_temporal_spd",
    store = store_h1
  )

output_temporal_events <-
  targets::tar_read(
    "output_temporal_events",
    store = store_h1
  )

store_spatiotemporal_control <-
  file.path(
    data_storage_path,
    "Targets_data",
    "analyses_h1_reviewer_spatiotemporal_control"
  )

data_spatial_control_zero_truncated_composition <-
  targets::tar_read(
    table_spatial_control_zero_truncated_composition,
    store = store_spatiotemporal_control
  )

data_spatial_control_hierarchical_contributions <-
  targets::tar_read(
    table_spatial_control_hierarchical_contributions,
    store = store_spatiotemporal_control
  )

data_spatial_control_unique_adjusted_r2 <-
  targets::tar_read(
    table_spatial_control_unique_adjusted_r2,
    store = store_spatiotemporal_control
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
    data_components = data_spatial_control_hierarchical_contributions,
    data_unique_adjusted_r2 = data_spatial_control_unique_adjusted_r2
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
      paste0(
        here::here(
          "Outputs/Figures/H1/Temporal/HVarPart/",
          paste0(
            "human_climate_space_",
            "zero_truncated_hierarchical_composition"
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
      filename = paste0(
        here::here(
          "Outputs/Figures/H1/Temporal/HVarPart/",
          "human_climate_composition_human_climate_only"
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
      paste0(
        here::here(
          "Outputs/Figures/H1/Temporal/HVarPart/",
          paste0(
            "human_climate_only_",
            "untruncated_hierarchical_contributions"
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
            "human_climate_space_",
            .y,
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
    "Outputs/Tables/",
    "human_climate_temporal_zero_truncated_hierarchical_composition.csv"
  )
)

readr::write_csv(
  data_temporal_untruncated,
  here::here(
    "Outputs/Tables/",
    "human_climate_temporal_untruncated_hierarchical_contributions.csv"
  )
)

readr::write_csv(
  data_temporal_balance,
  here::here("Outputs/Tables/human_climate_temporal_balance.csv")
)

readr::write_csv(
  data_spatial_control_zero_truncated_composition,
  here::here(
    "Outputs/Tables/",
    "human_climate_space_zero_truncated_hierarchical_composition.csv"
  )
)

readr::write_csv(
  data_spatial_control_hierarchical_contributions,
  here::here(
    "Outputs/Tables/",
    "human_climate_space_untruncated_hierarchical_contributions.csv"
  )
)

readr::write_csv(
  data_spatial_control_unique_adjusted_r2,
  here::here("Outputs/Tables/human_climate_space_unique_adjusted_r2.csv")
)
