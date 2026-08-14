#----------------------------------------------------------#
# H1 time-controlled and spatially aggregated SPD balance
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )

store_time_control <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/spd"
  )

runner_h1 <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"

list(
  targets::tar_target(
    name = fingerprint_time_control,
    command = compute_target_store_fingerprint(
      store = store_time_control,
      target_names = "data_time_controlled_balance_records",
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = data_time_controlled_balance_records,
    command = {
      fingerprint_time_control

      load_target_store_value(
        store = store_time_control,
        target_name = "data_time_controlled_balance_records",
        runner = runner_h1
      )
    }
  ),
  targets::tar_target(
    name = h1_analysis_config,
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_analysis_config",
      runner = runner_h1
    )
  ),
  targets::tar_target(
    name = output_spatiotemporal_balance_filter,
    command = fit_spatial_importance(
      data_records = data_time_controlled_balance_records,
      permutations = h1_analysis_config[["permutations"]],
      alpha = h1_analysis_config[["alpha"]],
      min_unique_locations =
        h1_analysis_config[["min_unique_locations"]],
      min_residual_df =
        h1_analysis_config[["min_spatial_residual_df"]],
      distance_km = h1_analysis_config[["spatial_distances_km"]],
      seed = h1_analysis_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_estimates,
    command = output_spatiotemporal_balance_filter[["estimates"]]
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_moran,
    command = output_spatiotemporal_balance_filter[["moran_diagnostics"]]
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_dbmem_diagnostics,
    command = output_spatiotemporal_balance_filter[["dbmem"]][[
      "diagnostics"
    ]]
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_dbmem_selection,
    command = tibble::tibble(
      status = output_spatiotemporal_balance_filter[[
        "selection"
      ]][["status"]],
      n_complete = output_spatiotemporal_balance_filter[[
        "selection"
      ]][["n_complete"]],
      n_candidates = output_spatiotemporal_balance_filter[[
        "selection"
      ]][["n_candidates"]],
      global_p_value = output_spatiotemporal_balance_filter[[
        "selection"
      ]][["global_p_value"]],
      full_adjusted_r_squared = output_spatiotemporal_balance_filter[[
        "selection"
      ]][["full_adjusted_r_squared"]],
      selected_terms = stringr::str_c(
        output_spatiotemporal_balance_filter[[
          "selection"
        ]][["selected_names"]],
        collapse = ";"
      )
    )
  )
)
