#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                 Prepare predictor data
#
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#



#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)


# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)


#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

list(
  # load events from _targets_events ----
  targets::tar_target(
    name = data_events_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_events/objects/events_temporal_subset"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = events_temporal_subset,
    command = get_file_from_path(data_events_path)
  ),
  # - file path to climate data ----
  targets::tar_target(
    name = file_climate_path,
    command = paste0(
      data_storage_path,
      "Data/climate/data_climate-2024-01-23.rds"
    ),
    format = "file"
  ),
  # - load climate data ----
  targets::tar_target(
    name = data_climate,
    command = get_file_from_path(file_climate_path)
  ),
  # - select climate variables ----
  targets::tar_target(
    name = data_climate_for_interpolation,
    command = get_climate_data_for_interpolation(
      data_source = data_climate,
      sel_var = c(
        "temp_annual",
        "temp_cold",
        "prec_annual",
        "prec_summer",
        "prec_win"
      )
    )
  ),
  # - interpolate climate values for each time slice ----
  targets::tar_target(
    name = data_climate_interpolated,
    command = get_interpolated_data(
      data_source = data_climate_for_interpolation,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - file path to spd data ----
  targets::tar_target(
    name = file_spd_path,
    command = paste0(
      data_storage_path,
      "Data/spd/data_spd-2024-01-29.rds"
    ),
    format = "file"
  ),
  # - load spd data  ----
  targets::tar_target(
    name = data_spd,
    command = get_file_from_path(file_spd_path)
  ),
  # - prepare spd for modelling ----
  targets::tar_target(
    name = data_spd_to_fit,
    command = get_spd_for_modelling(data_spd)
  ),
  # - interpolated spd values for each time slice ----
  targets::tar_target(
    name = data_spd_interpolated,
    command = get_interpolated_data(
      data_source = data_spd_to_fit,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - combine spd and human impact events ---- 
  targets::tar_target(
    name = data_spd_events,
    command = get_events_spd_combined(
      data_source_events = events_temporal_subset,
      data_source_spd = data_spd_interpolated,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  ),
  # - combine predictor data ----
  targets::tar_target(
    name = data_predictors,
    command = get_data_predictors(
      data_source_spd_events = data_spd_events,
      data_source_climate = data_climate_interpolated
    )
  )
 
)