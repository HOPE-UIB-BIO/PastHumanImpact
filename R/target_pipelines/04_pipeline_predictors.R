#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                 Prepare predictor data
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#



#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)


# - Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
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
      "Targets_data/pipeline_events/objects/events_temporal_subset"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = events_temporal_subset,
    command = resolve_file_path(data_events_path)
  ),
  # - file path to climate data ----
  targets::tar_target(
    name = file_climate_path,
    command = RUtilpol::get_latest_file_name(
      file_name = "data_climate",
      dir = paste0(
        data_storage_path,
        "Climate/"
      )
    ),
    format = "file"
  ),
  # - load climate data ----
  targets::tar_target(
    name = data_climate,
    command = resolve_file_path(file_climate_path)
  ),
  # - select climate variables ----
  targets::tar_target(
    name = data_climate_for_interpolation,
    command = prepare_climate_interpolation_data(
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
    command = prepare_interpolated_model_data(
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
    command = RUtilpol::get_latest_file_name(
      file_name = "data_spd_combine",
      dir = paste0(
        data_storage_path,
        "SPD/"
      )
    ),
    format = "file"
  ),
  # - load spd data  ----
  targets::tar_target(
    name = data_spd,
    command = resolve_file_path(file_spd_path)
  ),
  # - prepare spd for modelling ----
  targets::tar_target(
    name = data_spd_to_fit,
    command = prepare_spd_model_data(data_spd %>%
      dplyr::select(-distance))
  ),
  # - interpolated spd values for each time slice ----
  targets::tar_target(
    name = data_spd_interpolated,
    command = prepare_interpolated_model_data(
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
    command = aggregate_events_spd(
      data_source_events = events_temporal_subset,
      data_source_spd = data_spd_interpolated,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  ),
  # - combine predictor data ----
  targets::tar_target(
    name = data_predictors,
    command = prepare_predictor_data(
      data_source_spd_events = data_spd_events,
      data_source_climate = data_climate_interpolated
    )
  ),
  # - filter data properties for analyses ----
  targets::tar_target(
    name = data_predictors_filtered,
    command = prepare_filtered_hvarpart_data(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = 2000,
      age_to = 8500,
      remove_private = TRUE
    )
  )
)
