#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#          Human-climate spatial balance analysis
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

source(here::here("R/main_analysis/02_meta_data.R"))

#----------------------------------------------------------#
# 1. Load and extract fitted results -----
#----------------------------------------------------------#
output_spatial_spd <-
  targets::tar_read(
    name = "output_spatial_spd",
    store = paste0(data_storage_path, "Targets_data/analyses_h1")
  )

store_spatiotemporal_control <-
  file.path(
    data_storage_path,
    "Targets_data",
    "analyses_h1_reviewer_spatiotemporal_control"
  )

data_time_space_controlled_records <-
  targets::tar_read(
    data_time_controlled_balance_records,
    store = store_spatiotemporal_control
  )

data_time_space_controlled_estimates <-
  targets::tar_read(
    table_spatiotemporal_balance_estimates,
    store = store_spatiotemporal_control
  )

data_time_controlled_contributions <-
  targets::tar_read(
    table_time_control_hierarchical_contributions,
    store = store_spatiotemporal_control
  )

data_time_controlled_unique_adjusted_r2 <-
  targets::tar_read(
    table_time_control_unique_adjusted_r2,
    store = store_spatiotemporal_control
  )

data_importance <-
  compute_hvarpart_importance(
    data_source = output_spatial_spd |>
      dplyr::left_join(
        data_meta |>
          dplyr::select(dataset_id, region, climatezone),
        by = "dataset_id"
      ) |>
      dplyr::mutate(analysis = "spatial_spd"),
    id_cols = c("analysis", "dataset_id", "region", "climatezone")
  )

data_geo_koppen <-
  readr::read_rds(
    paste0(data_storage_path, "Spatial/Climatezones/data_geo_koppen.rds")
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

#----------------------------------------------------------#
# 2. Build balance main and signed supplementary figures -----
#----------------------------------------------------------#
plot_human_climate_only_balance <-
  plot_hvarpart_spatial_balance(
    data_importance = data_importance,
    data_meta = data_meta,
    data_geo_koppen = data_geo_koppen
  )

plot_time_space_controlled_balance <-
  plot_h1_spatial_controlled_balance(
    data_records = data_time_space_controlled_records,
    data_estimates = data_time_space_controlled_estimates,
    data_geo_koppen = data_geo_koppen
  )

plots_time_control_profiles <-
  plot_h1_spatial_control_profiles(
    data_records = data_time_space_controlled_records,
    data_components = data_time_controlled_contributions,
    data_unique_adjusted_r2 = data_time_controlled_unique_adjusted_r2
  )

plot_untruncated_spatial_contributions <-
  plot_untruncated_spatial_hvarpart(
    data_importance = data_importance,
    data_meta = data_meta,
    data_geo_koppen = data_geo_koppen
  )

#----------------------------------------------------------#
# 3. Save figures and source tables -----
#----------------------------------------------------------#
dir.create(
  here::here("Outputs/Tables/HVarPart"),
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  here::here("Outputs/Figures/H1/Spatial"),
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
          "Outputs/Figures/H1/Spatial/",
          "human_climate_balance_time_and_space_controlled"
        ),
        ".",
        extension
      ),
      plot = plot_time_space_controlled_balance,
      width = image_width_vec[["2col"]],
      height = 130,
      units = image_units,
      bg = "white"
    )

    ggplot2::ggsave(
      filename = paste0(
        here::here(
          "Outputs/Figures/H1/Spatial/",
          "human_climate_balance_human_climate_only"
        ),
        ".",
        .x
      ),
      plot = plot_human_climate_only_balance$plot,
      width = image_width_vec[["3col"]],
      height = 170,
      units = image_units,
      bg = "white"
    )

    ggplot2::ggsave(
      paste0(
        here::here(
          "Outputs/Figures/H1/Spatial/",
          paste0(
            "human_climate_balance_",
            "untruncated_hierarchical_contributions"
          )
        ),
        ".",
        extension
      ),
      plot = plot_untruncated_spatial_contributions$plot,
      width = image_width_vec[["3col"]],
      height = 180,
      units = image_units,
      bg = "white"
    )

    purrr::iwalk(
      .x = plots_time_control_profiles,
      .f = ~ ggplot2::ggsave(
        filename = file.path(
          here::here(
            "Outputs/Figures/H1/Spatial"
          ),
          stringr::str_c(
            "human_climate_time_",
            .y,
            ".",
            extension
          )
        ),
        plot = .x,
        width = image_width_vec[["2col"]],
        height = 190,
        units = image_units,
        bg = "white"
      )
    )
  }
)

readr::write_csv(
  plot_untruncated_spatial_contributions$record_values,
  here::here(
    "Outputs/Tables/HVarPart/",
    "human_climate_balance_untruncated_dataset_values.csv"
  )
)

readr::write_csv(
  plot_untruncated_spatial_contributions$climatezone_values,
  here::here(
    "Outputs/Tables/HVarPart/",
    "human_climate_balance_untruncated_climate_zone_values.csv"
  )
)

readr::write_csv(
  plot_untruncated_spatial_contributions$region_values,
  here::here(
    "Outputs/Tables/HVarPart/",
    "human_climate_balance_untruncated_region_values.csv"
  )
)

readr::write_csv(
  data_time_space_controlled_records,
  here::here(
    "Outputs/Tables/HVarPart/",
    "human_climate_balance_time_space_controlled_dataset_values.csv"
  )
)

readr::write_csv(
  data_time_space_controlled_estimates,
  here::here(
    "Outputs/Tables/HVarPart/",
    "human_climate_balance_time_space_controlled_climate_zone_values.csv"
  )
)
