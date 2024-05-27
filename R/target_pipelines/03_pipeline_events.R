#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#           Human events detection from pollen diagrams
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
  # get pollen data from _targets_data ----
  targets::tar_target(
    name = data_pollen_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_pollen_data/objects/data_pollen"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_pollen,
    command = get_file_from_path(data_pollen_path)
  ),
  # - a path for events from diagrams ----
  targets::tar_target(
    name = events_diag_path,
    command = paste0(
      data_storage_path,
      "Data/events/events_from_diagrams_2022-11-24.rds"
    ),
    format = "file"
  ),
  # - load events from diagrams ----
  targets::tar_target(
    name = events_diag_raw,
    command = get_file_from_path(events_diag_path)
  ),
  # - turn events from diagrams into binary ----
  targets::tar_target(
    name = events_diag_binary,
    command = get_events_as_binary(events_diag_raw, data_pollen)
  ),
  # add logical rules to the binary values ----
  targets::tar_target(
    name = events_diag,
    command = add_logical_rules(events_diag_binary)
  ),
  # - a path for indicators ----
  targets::tar_target(
    name = events_indicators_path,
    command = paste0(
      data_storage_path,
      "Data/events/events_from_code_indicators_2022-11-25.rds"
    ),
    format = "file"
  ),
  # - load indicators ----
  targets::tar_target(
    name = events_indicators_raw,
    command = get_file_from_path(events_indicators_path)
  ),
  # - detect indicators in data ----
  targets::tar_target(
    name = events_indicators,
    command = get_events_from_indicators(
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
  targets::tar_target(
    name = events_indices_path,
    command = paste0(
      data_storage_path,
      "Data/events/events_from_code_indices_2022-11-25.rds"
    ),
    format = "file"
  ),
  # - load indices ----
  targets::tar_target(
    name = events_indices_raw,
    command = get_file_from_path(events_indices_path)
  ),
  # - detect indices in data ----
  targets::tar_target(
    name = events_indices,
    command = get_events_from_indices(
      data_source_indices = events_indices_raw,
      data_source_pollen = data_pollen,
      data_source_meta = data_meta,
      sel_region = "Latin America"
    )
  ),
  # - merge all events detected by code together ----
  targets::tar_target(
    name = events_code,
    command = merge_indicators_and_indices(
      data_source_indices = events_indices,
      data_source_indicators = events_indicators
    )
  ),
  # - merge all events together ----
  targets::tar_target(
    name = events,
    command = merge_all_events(
      data_source_events_diag = events_diag,
      data_source_events_code = events_code
    )
  ),
  # - prepare events for modelling ----
  targets::tar_target(
    name = data_events_to_fit,
    command = get_events_for_modelling(events)
  ),
  # - interpolate data for even time steps ----
  targets::tar_target(
    name = events_interpolated,
    command = get_interpolated_data(
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
  targets::tar_target(
    name = events_temporal_subset,
    command = subset_event_types(
      data_source_events = events_interpolated,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time
    )
  )
)
