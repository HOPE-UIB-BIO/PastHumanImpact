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
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)


#----------------------------------------------------------#
# 1. Upstream contract -----
#----------------------------------------------------------#

store_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/events"
  )

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

#----------------------------------------------------------#
# 2. Targets -----
#----------------------------------------------------------#

list(
  targets::tar_target(
    name = fingerprint_events,
    command = compute_target_store_fingerprint(
      store = store_events,
      target_names = "events_temporal_subset",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = events_temporal_subset,
    command = {
      fingerprint_events

      load_target_store_value(
        store = store_events,
        target_name = "events_temporal_subset",
        runner = runner_data_preparation
      )
    }
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
