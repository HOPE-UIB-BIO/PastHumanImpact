#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#             Spatial aggregation of the event H1 balance
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the spatial aggregation of the event H1 balance target graph.
# Run with:
#   R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
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
    store_relative_path = "analyses_h1/time_control/events"
  )

runner_h1 <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint event time control so upstream changes invalidate this
  #   pipeline store without refitting the established temporal models.
  targets::tar_target(
    name = "fingerprint_time_control",
    command = compute_target_store_fingerprint(
      store = store_time_control,
      target_names = c(
        "table_time_control_hierarchical_contributions",
        "table_time_control_status"
      ),
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint metadata and configuration so spatial inputs invalidate
  #   this store when their public source targets change.
  targets::tar_target(
    name = "fingerprint_h1_inputs",
    command = compute_target_store_fingerprint(
      store = store_inputs,
      target_names = c("data_meta", "h1_analysis_config"),
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Load time-controlled event contributions so the spatial balance uses
  #   the established reciprocal H1 models without rerunning them.
  targets::tar_target(
    name = "table_time_control_hierarchical_contributions",
    command = {
      fingerprint_time_control

      load_target_store_value(
        store = store_time_control,
        target_name = "table_time_control_hierarchical_contributions",
        runner = runner_h1
      )
    }
  ),
  # Why: Load event model statuses so only estimable time-controlled results
  #   enter spatial aggregation.
  targets::tar_target(
    name = "table_time_control_status",
    command = load_target_store_value(
      store = store_time_control,
      target_name = "table_time_control_status",
      runner = runner_h1
    )
  ),
  # Why: Load dataset metadata so event models receive coordinates and the
  #   canonical spatial strata.
  targets::tar_target(
    name = "data_meta",
    command = {
      fingerprint_h1_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_meta",
        runner = runner_h1
      )
    }
  ),
  # Why: Load H1 settings so event filtering uses the canonical thresholds,
  #   permutations, and seed.
  targets::tar_target(
    name = "h1_analysis_config",
    command = load_target_store_value(
      store = store_inputs,
      target_name = "h1_analysis_config",
      runner = runner_h1
    )
  ),
  # Why: Prepare every estimable time-controlled event record so exclusions
  #   remain auditable before the spatial eligibility filter.
  targets::tar_target(
    name = "data_time_controlled_balance_records_all",
    command = prepare_time_controlled_importance_records(
      data_components = table_time_control_hierarchical_contributions,
      data_status = table_time_control_status,
      data_meta = data_meta
    )
  ),
  # Why: Retain finite, positively weighted event balances so both spatial
  #   profiles use the same eligible dataset set.
  targets::tar_target(
    name = "data_time_controlled_balance_records",
    command = data_time_controlled_balance_records_all |>
      dplyr::filter(
        is.finite(.data[["signed_difference"]]),
        .data[["signed_weight"]] > 0,
        is.finite(.data[["zero_balance"]]),
        .data[["zero_weight"]] > 0
      )
  ),
  # Why: Fit the event balance filter once so untruncated and zero-truncated
  #   summaries reuse the same selected spatial terms.
  targets::tar_target(
    name = "output_spatiotemporal_balance_filter",
    command = fit_spatial_importance(
      data_records = data_time_controlled_balance_records,
      permutations = h1_analysis_config[["permutations"]],
      alpha = h1_analysis_config[["alpha"]],
      min_unique_locations =
        h1_analysis_config[["min_unique_locations"]],
      min_residual_df =
        h1_analysis_config[["min_spatial_residual_df"]],
      distance_km = h1_analysis_config[["spatial_distances_km"]],
      seed = h1_analysis_config[["seed"]] + 10000L
    )
  ),
  # Why: Materialize event balance estimates so figures and reports share one
  #   auditable spatially adjusted table.
  targets::tar_target(
    name = "table_spatiotemporal_balance_estimates",
    command = output_spatiotemporal_balance_filter[["estimates"]]
  ),
  # Why: Materialize event Moran diagnostics so unresolved spatial dependence
  #   remains visible.
  targets::tar_target(
    name = "table_spatiotemporal_balance_moran",
    command = output_spatiotemporal_balance_filter[["moran_diagnostics"]]
  ),
  # Why: Materialize event dbMEM diagnostics so spatial-network eligibility is
  #   available for provenance and review.
  targets::tar_target(
    name = "table_spatiotemporal_balance_dbmem_diagnostics",
    command = output_spatiotemporal_balance_filter[["dbmem"]][[
      "diagnostics"
    ]]
  ),
  # Why: Materialize event dbMEM selection so the retained spatial terms and
  #   stopping statistics are explicit.
  targets::tar_target(
    name = "table_spatiotemporal_balance_dbmem_selection",
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
