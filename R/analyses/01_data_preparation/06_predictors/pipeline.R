#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Predictor data preparation
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the predictor data preparation target graph.
# Run with:
#   R/analyses/01_data_preparation/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
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
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint events so upstream changes invalidate this pipeline store.
  targets::tar_target(
    name = "fingerprint_events",
    command = compute_target_store_fingerprint(
      store = store_events,
      target_names = "events_temporal_subset",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Define events temporal subset once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_temporal_subset",
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
  # Why: Track climate path as a file target so file changes invalidate
  #   downstream results.
  targets::tar_target(
    name = "file_climate_path",
    command = resolve_latest_file_path(
      file_name = "data_climate",
      dir = paste0(
        data_storage_path,
        "Climate/"
      )
    ),
    format = "file"
  ),
  # - load climate data ----
  # Why: Prepare climate so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_climate",
    command = resolve_file_path(file_climate_path)
  ),
  # - select climate variables ----
  # Why: Prepare climate for interpolation so downstream targets share one
  #   canonical dataset.
  targets::tar_target(
    name = "data_climate_for_interpolation",
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
  # Why: Prepare climate interpolated so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_climate_interpolated",
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
  # Why: Track SPD path as a file target so file changes invalidate downstream
  #   results.
  targets::tar_target(
    name = "file_spd_path",
    command = resolve_latest_file_path(
      file_name = "data_spd_combine",
      dir = paste0(
        data_storage_path,
        "SPD/"
      )
    ),
    format = "file"
  ),
  # - load spd data  ----
  # Why: Prepare SPD so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_spd",
    command = resolve_file_path(file_spd_path)
  ),
  # - prepare spd for modelling ----
  # Why: Prepare SPD to fit so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_spd_to_fit",
    command = prepare_spd_model_data(data_spd %>%
      dplyr::select(-distance))
  ),
  # - interpolated spd values for each time slice ----
  # Why: Prepare SPD interpolated so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_spd_interpolated",
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
  # Why: Prepare SPD events so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_spd_events",
    command = aggregate_events_spd(
      data_source_events = events_temporal_subset,
      data_source_spd = data_spd_interpolated,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  ),
  # - combine predictor data ----
  # Why: Prepare predictors so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_predictors",
    command = prepare_predictor_data(
      data_source_spd_events = data_spd_events,
      data_source_climate = data_climate_interpolated
    )
  ),
  # - filter data properties for analyses ----
  # Why: Prepare predictors filtered so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_predictors_filtered",
    command = prepare_filtered_hvarpart_data(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = 2000,
      age_to = 8500,
      remove_private = TRUE
    )
  )
)
