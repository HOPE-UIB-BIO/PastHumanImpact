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

source(
  here::here(
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)

#----------------------------------------------------------#
# 1. Load controlled results -----
#----------------------------------------------------------#
store_time_control <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/spd"
  )

store_spatial_aggregation <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "analyses_h1/spatial_aggregation",
      "spd_human_climate_balance",
      sep = "/"
    )
  )

data_time_space_controlled_records <-
  targets::tar_read(
    data_time_controlled_balance_records,
    store = store_time_control
  )

data_time_space_controlled_estimates <-
  targets::tar_read(
    table_spatiotemporal_balance_estimates,
    store = store_spatial_aggregation
  )

data_time_controlled_contributions <-
  targets::tar_read(
    table_time_control_hierarchical_contributions,
    store = store_time_control
  )

data_time_controlled_unique_adjusted_r2 <-
  targets::tar_read(
    table_time_control_unique_adjusted_r2,
    store = store_time_control
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
# 2. Build the main balance and component figures -----
#----------------------------------------------------------#
plot_time_space_controlled_balance <-
  plot_h1_spatial_controlled_balance(
    data_records = data_time_space_controlled_records,
    data_estimates = data_time_space_controlled_estimates,
    data_geo_koppen = data_geo_koppen
  )

plots_time_control_profiles <-
  build_h1_spatial_control_profiles(
    data_records = data_time_space_controlled_records,
    data_components = data_time_controlled_contributions,
    data_unique_adjusted_r2 = data_time_controlled_unique_adjusted_r2,
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
  c("png", "pdf"),
  ~ {
    extension <- .x

    ggplot2::ggsave(
      filename = paste0(
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

    purrr::iwalk(
      plots_time_control_profiles,
      ~ ggplot2::ggsave(
        filename = file.path(
          here::here("Outputs/Figures/H1/Spatial"),
          paste0(.y, ".", extension)
        ),
        plot = .x[["plot"]],
        width = image_width_vec[["2col"]],
        height = 130,
        units = image_units,
        bg = "white"
      )
    )
  }
)

data_profile_records <-
  plots_time_control_profiles |>
  purrr::imap_dfr(
    ~ .x[["record_values"]] |>
      dplyr::mutate(figure_profile = .y)
  )

data_profile_climatezones <-
  plots_time_control_profiles |>
  purrr::imap_dfr(
    ~ .x[["climatezone_values"]] |>
      dplyr::mutate(figure_profile = .y)
  )

data_profile_regions <-
  plots_time_control_profiles |>
  purrr::imap_dfr(
    ~ .x[["region_values"]] |>
      dplyr::mutate(figure_profile = .y)
  )

readr::write_csv(
  data_profile_records,
  here::here(
    "Outputs/Tables/HVarPart/",
    "time_controlled_spatial_component_dataset_values.csv"
  )
)

readr::write_csv(
  data_profile_climatezones,
  here::here(
    "Outputs/Tables/HVarPart/",
    "time_controlled_spatial_component_climate_zone_values.csv"
  )
)

readr::write_csv(
  data_profile_regions,
  here::here(
    "Outputs/Tables/HVarPart/",
    "time_controlled_spatial_component_region_values.csv"
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
