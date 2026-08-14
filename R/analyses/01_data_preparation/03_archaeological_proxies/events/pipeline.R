#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Archaeological events preparation
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the archaeological events preparation target graph.
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

store_pollen <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/pollen"
  )

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint pollen so upstream changes invalidate this pipeline store.
  targets::tar_target(
    name = "fingerprint_pollen",
    command = compute_target_store_fingerprint(
      store = store_pollen,
      target_names = "data_pollen",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Prepare pollen so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_pollen",
    command = {
      fingerprint_pollen

      load_target_store_value(
        store = store_pollen,
        target_name = "data_pollen",
        runner = runner_data_preparation
      )
    }
  ),
  # - a path for events from diagrams ----
  # Why: Define events diag path once so downstream targets use one reproducible
  #   value.
  targets::tar_target(
    name = "events_diag_path",
    command = resolve_latest_file_path(
      file_name = "events_from_diagrams",
      dir = paste0(
        data_storage_path,
        "Events/"
      )
    ),
    format = "file"
  ),
  # - load events from diagrams ----
  # Why: Define events diag raw once so downstream targets use one reproducible
  #   value.
  targets::tar_target(
    name = "events_diag_raw",
    command = resolve_file_path(events_diag_path)
  ),
  # - turn events from diagrams into binary ----
  # Why: Define events diag binary once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_diag_binary",
    command = classify_binary_events(events_diag_raw, data_pollen)
  ),
  # add logical rules to the binary values ----
  # Why: Define events diag once so downstream targets use one reproducible
  #   value.
  targets::tar_target(
    name = "events_diag",
    command = classify_events_by_logical_rules(events_diag_binary)
  ),
  # - a path for indicators ----
  # Why: Define events indicators path once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_indicators_path",
    command = resolve_latest_file_path(
      file_name = "events_from_code_indicators",
      dir = paste0(
        data_storage_path,
        "Events/"
      )
    ),
    format = "file"
  ),
  # - load indicators ----
  # Why: Define events indicators raw once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_indicators_raw",
    command = resolve_file_path(events_indicators_path)
  ),
  # - detect indicators in data ----
  # Why: Define events indicators once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_indicators",
    command = classify_indicator_events(
      data_source_indicators = events_indicators_raw,
      data_source_pollen = data_pollen,
      data_source_meta = data_meta,
      sel_region = "Latin America",
      # filter out pinus in selected  countries where Pinus is native
      country_w_pinus = c(
        "Mexico",
        "Guatemala",
        "Honduras",
        "Nicaragua",
        "Costa Rica"
      )
    )
  ),
  # - a path for indices ----
  # Why: Define events indices path once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_indices_path",
    command = resolve_latest_file_path(
      file_name = "events_from_code_indices",
      dir = paste0(
        data_storage_path,
        "Events/"
      )
    ),
    format = "file"
  ),
  # - load indices ----
  # Why: Define events indices raw once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_indices_raw",
    command = resolve_file_path(events_indices_path)
  ),
  # - detect indices in data ----
  # Why: Define events indices once so downstream targets use one reproducible
  #   value.
  targets::tar_target(
    name = "events_indices",
    command = classify_index_events(
      data_source_indices = events_indices_raw,
      data_source_pollen = data_pollen,
      data_source_meta = data_meta,
      sel_region = "Latin America"
    )
  ),
  # - merge all events detected by code together ----
  # Why: Define events code once so downstream targets use one reproducible
  #   value.
  targets::tar_target(
    name = "events_code",
    command = aggregate_event_sources(
      data_source_indices = events_indices,
      data_source_indicators = events_indicators
    )
  ),
  # - merge all events together ----
  # Why: Define events once so downstream targets use one reproducible value.
  targets::tar_target(
    name = "events",
    command = aggregate_events(
      data_source_events_diag = events_diag,
      data_source_events_code = events_code
    )
  ),
  # - prepare events for modelling ----
  # Why: Prepare events to fit so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_events_to_fit",
    command = prepare_event_model_data(events)
  ),
  # - interpolate data for even time steps ----
  # Why: Define events interpolated once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_interpolated",
    command = prepare_interpolated_model_data(
      data_source = data_events_to_fit,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "constant",
      rule = 1,
      ties = "ordered",
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - subset event types relevant for each region ----
  # Why: Define events temporal subset once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "events_temporal_subset",
    command = filter_event_types(
      data_source_events = events_interpolated,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  )
)
