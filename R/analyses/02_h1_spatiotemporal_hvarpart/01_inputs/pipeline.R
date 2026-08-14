#----------------------------------------------------------#
# H1 shared spatiotemporal inputs
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_profiles <-
  here::here(
    "R",
    "analyses",
    "00_profiles",
    "analysis_profiles.csv"
  )

store_paps <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/paps"
  )

store_predictors <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/predictors"
  )

runner_paps <-
  "R/analyses/01_data_preparation/00_run.R"

runner_predictors <-
  "R/analyses/01_data_preparation/00_run.R"

response_vars_h1 <-
  c(
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "roc",
    "dcca_axis_1",
    "density_diversity",
    "density_turnover"
  )

predictor_sets_h1 <-
  list(
    spd = list(
      human = "spd",
      climate = c(
        "temp_annual",
        "temp_cold",
        "prec_summer",
        "prec_win"
      )
    ),
    all_event_groups = list(
      human = c(
        "fi",
        "fc",
        "ec",
        "cc",
        "es",
        "ei",
        "weak",
        "medium",
        "strong"
      ),
      climate = c(
        "temp_annual",
        "temp_cold",
        "prec_summer",
        "prec_win"
      )
    )
  )

list(
  targets::tar_target(
    name = file_analysis_profiles,
    command = path_profiles,
    format = "file"
  ),
  targets::tar_target(
    name = data_analysis_profiles,
    command = load_analysis_profiles(file_analysis_profiles)
  ),
  targets::tar_target(
    name = h1_analysis_config,
    command = list(
      seed = as.integer(set_seed),
      permutations = 999L,
      alpha = 0.05,
      spatial_distances_km = c(250, 500),
      temporal_distances_years = c(500, 1000),
      min_unique_locations = 20L,
      min_spatial_residual_df = 10L,
      min_unique_ages = 10L,
      min_temporal_residual_df = 5L,
      thinning_repetitions = 100L
    )
  ),
  targets::tar_target(
    name = h1_response_variables,
    command = response_vars_h1
  ),
  targets::tar_target(
    name = h1_predictor_sets,
    command = predictor_sets_h1
  ),
  targets::tar_target(
    name = fingerprint_paps,
    command = compute_target_store_fingerprint(
      store = store_paps,
      target_names = c("data_properties", "data_properties_filtered"),
      runner = runner_paps
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = fingerprint_predictors,
    command = compute_target_store_fingerprint(
      store = store_predictors,
      target_names = c("data_predictors", "data_predictors_filtered"),
      runner = runner_predictors
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = data_properties,
    command = {
      fingerprint_paps

      load_target_store_value(
        store = store_paps,
        target_name = "data_properties",
        runner = runner_paps
      )
    }
  ),
  targets::tar_target(
    name = data_properties_filtered,
    command = {
      fingerprint_paps

      load_target_store_value(
        store = store_paps,
        target_name = "data_properties_filtered",
        runner = runner_paps
      )
    }
  ),
  targets::tar_target(
    name = data_predictors,
    command = {
      fingerprint_predictors

      load_target_store_value(
        store = store_predictors,
        target_name = "data_predictors",
        runner = runner_predictors
      )
    }
  ),
  targets::tar_target(
    name = data_predictors_filtered,
    command = {
      fingerprint_predictors

      load_target_store_value(
        store = store_predictors,
        target_name = "data_predictors_filtered",
        runner = runner_predictors
      )
    }
  ),
  targets::tar_target(
    name = data_meta_path,
    command = file.path(
      data_storage_path,
      "Assembly",
      RUtilpol::get_latest_file_name(
        file_name = "data_meta",
        dir = file.path(data_storage_path, "Assembly")
      )
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_meta,
    command = resolve_file_path(data_meta_path)
  ),
  targets::tar_target(
    name = data_hvar_filtered,
    command = prepare_combined_data(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  targets::tar_target(
    name = data_properties_temporal,
    command = prepare_filtered_hvarpart_data(
      data_source = data_properties,
      data_meta = data_meta,
      age_from = 0,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  targets::tar_target(
    name = data_predictors_temporal,
    command = prepare_filtered_hvarpart_data(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = 0,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  targets::tar_target(
    name = data_hvar_temporal,
    command = prepare_combined_data(
      data_source_properties = data_properties_temporal,
      data_source_predictors = data_predictors_temporal
    )
  ),
  targets::tar_target(
    name = output_dataset_age_collapse_filtered,
    command = aggregate_hvar_dataset_ages(
      data_source = data_hvar_filtered,
      response_vars = h1_response_variables,
      predictor_vars = unique(
        unlist(h1_predictor_sets, use.names = FALSE)
      )
    )
  ),
  targets::tar_target(
    name = data_hvar_filtered_unique_age,
    command = output_dataset_age_collapse_filtered[["data"]]
  ),
  targets::tar_target(
    name = table_dataset_age_collapse_filtered,
    command = output_dataset_age_collapse_filtered[["audit"]]
  ),
  targets::tar_target(
    name = output_dataset_age_collapse_temporal,
    command = aggregate_hvar_dataset_ages(
      data_source = data_hvar_temporal,
      response_vars = h1_response_variables,
      predictor_vars = unique(
        unlist(h1_predictor_sets, use.names = FALSE)
      )
    )
  ),
  targets::tar_target(
    name = data_hvar_temporal_unique_age,
    command = output_dataset_age_collapse_temporal[["data"]]
  ),
  targets::tar_target(
    name = table_dataset_age_collapse_temporal,
    command = output_dataset_age_collapse_temporal[["audit"]]
  ),
  targets::tar_target(
    name = data_hvar_timebins_unique_age,
    command = prepare_hvarpart_timebin_data(
      data_source = data_hvar_temporal_unique_age,
      data_meta = data_meta
    )
  ),
  targets::tar_target(
    name = data_hvar_timebins_spd_unique_age,
    command = data_hvar_timebins_unique_age |>
      dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500))
  )
)
