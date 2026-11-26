#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#       Single-proxy spatial H1: time-controlled HVarPart per
#        record with a shared climate selection, aggregated
#                  with dbMEM spatial control
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines one single-proxy spatial H1 run.
# Run with:
#   R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# One script serves every within-dataset profile with configuration
#   reference human_proxy_h1. The profile is declared per store in the
#   script_profile column of R/analyses/00_profiles/pipeline_contracts.csv
#   and is resolved from the store the runner passes; calls without a
#   registered store, such as targets::tar_manifest(), see the default
#   profile.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

data_pipeline_contracts <-
  readr::read_csv(
    here::here("R", "analyses", "00_profiles", "pipeline_contracts.csv"),
    show_col_types = FALSE
  )

profile_id_h1_proxy <-
  resolve_pipeline_profile(
    data_contracts = data_pipeline_contracts,
    script = file.path(
      "R", "analyses", "02_h1_spatiotemporal_hvarpart", "human_proxies",
      "02_time_control", "pipeline.R"
    ),
    store = targets::tar_path_store(),
    data_storage_path = data_storage_path,
    default_profile = "within_dataset_spd_spd_buffer_common_cohort_time_control"
  )

# NULL keeps every region; a character vector restricts the run to those
#   continental regions (trial runs only).
region_subset <- NULL

path_tables_root <-
  here::here("Outputs", "Tables", "H1", "Proxies")

path_tables <-
  file.path(path_tables_root, profile_id_h1_proxy)

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/human_proxy_inputs"
  )

runner_inputs <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R"

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint the shared inputs so changes to units, climate
  #   selections, or settings invalidate this run.
  targets::tar_target(
    name = "fingerprint_h1_proxy_inputs",
    command = compute_target_store_fingerprint(
      store = store_inputs,
      target_names = c(
        "data_h1_proxy_specifications",
        "h1_proxy_config",
        "h1_proxy_response_variables",
        "data_h1_proxy_meta",
        "data_h1_proxy_record_units",
        "data_h1_proxy_record_climate"
      ),
      runner = runner_inputs
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Select this store's profile: its human block, window, and support.
  targets::tar_target(
    name = "h1_proxy_specification",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_specifications",
        runner = runner_inputs
      ) |>
        dplyr::filter(.data[["model_id"]] == profile_id_h1_proxy)
    }
  ),
  # Why: Stop early unless the store resolves to one within-dataset profile.
  targets::tar_target(
    name = "check_h1_proxy_specification",
    command = assertthat::assert_that(
      nrow(h1_proxy_specification) == 1L,
      all(
        dplyr::pull(h1_proxy_specification, "analytical_unit") ==
          "within_dataset"
      ),
      msg = "The store does not resolve to one within-dataset profile."
    )
  ),
  # Why: Import the shared seed, thresholds, and bootstrap settings.
  targets::tar_target(
    name = "h1_proxy_config",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "h1_proxy_config",
        runner = runner_inputs
      )
    }
  ),
  # Why: Import the shared response columns.
  targets::tar_target(
    name = "h1_proxy_response_variables",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "h1_proxy_response_variables",
        runner = runner_inputs
      )
    }
  ),
  # Why: Import record regions, climate zones, and coordinates.
  targets::tar_target(
    name = "data_h1_proxy_meta",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_meta",
        runner = runner_inputs
      )
    }
  ),
  # Why: Keep the records of the selected regions (every region unless a
  #   trial run sets region_subset).
  targets::tar_target(
    name = "data_h1_proxy_region_records",
    command = data_h1_proxy_meta |>
      dplyr::filter(
        .data[["region"]] %in% (region_subset %||% .data[["region"]])
      ) |>
      dplyr::select(dplyr::all_of("dataset_id"))
  ),
  # Why: Import the record histories of this profile's window.
  targets::tar_target(
    name = "data_h1_proxy_units",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_record_units",
        runner = runner_inputs
      ) |>
        dplyr::filter(
          .data[["window_id"]] ==
            dplyr::pull(h1_proxy_specification, "window_id"),
          check_h1_proxy_specification
        ) |>
        dplyr::semi_join(data_h1_proxy_region_records, by = "dataset_id")
    }
  ),
  # Why: Import the climate block selected once for each of these records.
  targets::tar_target(
    name = "data_h1_proxy_climate",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_record_climate",
        runner = runner_inputs
      ) |>
        dplyr::semi_join(
          data_h1_proxy_units,
          by = c("window_id", "dataset_id")
        )
    }
  ),
  # Why: Attach this profile's human block to the shared climate block of
  #   every record.
  targets::tar_target(
    name = "data_h1_proxy_designs",
    command = prepare_shared_climate_hvarpart_designs(
      data_units = data_h1_proxy_units |>
        dplyr::select(dplyr::all_of(c("dataset_id", "data_merge"))),
      data_climate_selection = data_h1_proxy_climate |>
        dplyr::select(-dplyr::all_of("window_id")),
      model_specifications = h1_proxy_specification,
      unit_columns = "dataset_id",
      drop_rows_without_human = FALSE,
      max_cor = h1_proxy_config[["max_cor"]],
      max_vif = h1_proxy_config[["max_vif"]]
    )
  ),
  # Why: Fit one time-controlled HVarPart model per record with the
  #   unchanged single-record fitter.
  targets::tar_target(
    name = "output_h1_proxy_fits",
    command = fit_local_temporal_hvarpart_models(
      data_designs = data_h1_proxy_designs,
      response_vars = h1_proxy_response_variables,
      seed = h1_proxy_config[["seed"]],
      min_unique_ages = h1_proxy_config[["min_unique_ages"]],
      min_residual_df = h1_proxy_config[["min_temporal_residual_df"]],
      distance_years = h1_proxy_config[["temporal_distances_years"]],
      permutations = h1_proxy_config[["permutations"]]
    )
  ),
  # Why: Stop on any caught fit error instead of reporting partial results.
  targets::tar_target(
    name = "check_h1_proxy_model_errors",
    command = output_h1_proxy_fits |>
      dplyr::filter(!is.na(.data[["error_message"]])) |>
      nrow() |>
      magrittr::equals(0L) |>
      assertthat::assert_that(
        msg = "HVarPart model errors occurred; see output_h1_proxy_fits."
      )
  ),
  # Why: Extract status, contribution, and unique-fraction tables.
  targets::tar_target(
    name = "result_h1_proxy_time_control",
    command = {
      check_h1_proxy_model_errors

      summarise_local_temporal_hvarpart_models(
        data_results = output_h1_proxy_fits,
        analysis_prefix = "spatial_h1_human_proxy"
      )
    }
  ),
  # Why: Publish an explicit status for every record of the run.
  targets::tar_target(
    name = "table_h1_proxy_status",
    command = result_h1_proxy_time_control[["status"]] |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Keep the hierarchical contributions of every estimated record.
  targets::tar_target(
    name = "table_h1_proxy_components",
    command = result_h1_proxy_time_control[["components"]] |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Convert contributions to record-level human-climate balances.
  targets::tar_target(
    name = "data_h1_proxy_balance_records",
    command = prepare_time_controlled_importance_records(
      data_components = result_h1_proxy_time_control[["components"]],
      data_status = result_h1_proxy_time_control[["status"]],
      data_meta = data_h1_proxy_meta
    ) |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Publish the estimable record balances used by the aggregation and
  #   by cross-proxy comparisons.
  targets::tar_target(
    name = "data_h1_proxy_estimable_units",
    command = filter_estimable_balance_records(
      data_records = data_h1_proxy_balance_records
    )
  ),
  # Why: Publish the input records of the run for the identical-units check.
  targets::tar_target(
    name = "data_h1_proxy_run_units",
    command = data_h1_proxy_designs |>
      dplyr::transmute(
        profile_id = profile_id_h1_proxy,
        window_id = dplyr::pull(h1_proxy_specification, "window_id"),
        dataset_id = .data[["dataset_id"]]
      )
  ),
  # Why: Publish the climate block each record used, for the shared-climate
  #   check across runs.
  targets::tar_target(
    name = "table_h1_proxy_climate_used",
    command = data_h1_proxy_designs |>
      dplyr::transmute(
        profile_id = profile_id_h1_proxy,
        window_id = dplyr::pull(h1_proxy_specification, "window_id"),
        dataset_id = .data[["dataset_id"]],
        climate_set = .data[["climate_set"]],
        selection_status = .data[["selection_status"]],
        n_rows_without_human = .data[["n_rows_without_human"]]
      )
  ),
  # Why: Report eligibility and residual df with the selected versus all
  #   four climate variables.
  targets::tar_target(
    name = "table_h1_proxy_climate_reduction_eligibility",
    command = diagnose_climate_reduction_eligibility(
      data_designs = data_h1_proxy_designs,
      response_vars = h1_proxy_response_variables,
      climate_candidates = h1_proxy_config[["climate_candidates"]],
      min_unique_ages = h1_proxy_config[["min_unique_ages"]],
      min_residual_df = h1_proxy_config[["min_temporal_residual_df"]]
    ) |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Summarise that eligibility comparison by status and residual df.
  targets::tar_target(
    name = "table_h1_proxy_climate_reduction_summary",
    command = table_h1_proxy_climate_reduction_eligibility |>
      dplyr::count(
        .data[["profile_id"]],
        .data[["climate_set"]],
        .data[["status"]],
        .data[["residual_df"]],
        name = "n_records"
      )
  ),
  # Why: Aggregate the record balances with dbMEM spatial control and
  #   bootstrap the overall balance of each profile (decision metric).
  targets::tar_target(
    name = "output_h1_proxy_decision_metric",
    command = fit_human_proxy_spatial_decision_metric(
      data_records = data_h1_proxy_estimable_units,
      n_bootstrap = h1_proxy_config[["n_bootstrap"]],
      seed = h1_proxy_config[["seed"]],
      permutations = h1_proxy_config[["permutations"]],
      alpha = h1_proxy_config[["alpha"]],
      min_unique_locations = h1_proxy_config[["min_unique_locations"]],
      min_residual_df = h1_proxy_config[["min_spatial_residual_df"]],
      distance_km = h1_proxy_config[["spatial_distances_km"]]
    )
  ),
  # Why: Publish the decision metric with the run identifiers.
  targets::tar_target(
    name = "table_h1_proxy_decision_metric",
    command = output_h1_proxy_decision_metric[["metric"]] |>
      dplyr::mutate(
        profile_id = profile_id_h1_proxy,
        analysis = "spatial_h1",
        human_proxy = dplyr::pull(h1_proxy_specification, "human_proxy"),
        proxy_support = dplyr::pull(h1_proxy_specification, "proxy_support"),
        age_window = dplyr::pull(h1_proxy_specification, "age_window"),
        window_id = dplyr::pull(h1_proxy_specification, "window_id"),
        .before = 1L
      )
  ),
  # Why: Publish the overall, regional, and stratum adjusted balances.
  targets::tar_target(
    name = "table_h1_proxy_balance_estimates",
    command = output_h1_proxy_decision_metric[["estimates"]] |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Export every run table to the run's own output folder.
  targets::tar_target(
    name = "files_h1_proxy_tables",
    command = {
      list_tables <-
        list(
          human_proxy__model_status__time_control = table_h1_proxy_status,
          human_proxy__record_balances__time_control =
            data_h1_proxy_balance_records,
          human_proxy__hierarchical_contributions__time_control =
            table_h1_proxy_components,
          human_proxy__unique_adjusted_r2__time_control =
            result_h1_proxy_time_control[["unique_adjusted_r2"]],
          human_proxy__climate_selection__time_control =
            table_h1_proxy_climate_used,
          human_proxy__climate_reduction_eligibility__time_control =
            table_h1_proxy_climate_reduction_eligibility,
          human_proxy__climate_reduction_summary__time_control =
            table_h1_proxy_climate_reduction_summary,
          human_proxy__balance_estimates__time_and_space_control =
            table_h1_proxy_balance_estimates,
          human_proxy__decision_metric__time_and_space_control =
            table_h1_proxy_decision_metric,
          human_proxy__balance_moran__time_and_space_control =
            output_h1_proxy_decision_metric[["moran_diagnostics"]]
        )

      vec_paths <-
        file.path(path_tables, stringr::str_c(names(list_tables), ".csv")) |>
        rlang::set_names(names(list_tables))

      save_collinearity_managed_hvarpart_tables(list_tables, vec_paths)
    },
    format = "file"
  )
)
