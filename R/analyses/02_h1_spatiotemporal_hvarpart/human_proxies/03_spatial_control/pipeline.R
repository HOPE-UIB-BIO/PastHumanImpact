#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#      Single-proxy temporal H1: spatially controlled HVarPart
#      per continental region and age with a shared climate
#                          selection
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines one single-proxy temporal H1 run.
# Run with:
#   R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# One script serves every time-slice profile with configuration reference
#   human_proxy_h1. The profile is declared per store in the script_profile
#   column of R/analyses/00_profiles/pipeline_contracts.csv and is resolved
#   from the store the runner passes; calls without a registered store, such
#   as targets::tar_manifest(), see the default profile.

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
      "03_spatial_control", "pipeline.R"
    ),
    store = targets::tar_path_store(),
    data_storage_path = data_storage_path,
    default_profile = "time_slice_spd_spd_buffer_common_cohort_spatial_control"
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
        "data_h1_proxy_slice_units",
        "data_h1_proxy_slice_climate"
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
  # Why: Stop early unless the store resolves to one time-slice profile.
  targets::tar_target(
    name = "check_h1_proxy_specification",
    command = assertthat::assert_that(
      nrow(h1_proxy_specification) == 1L,
      all(
        dplyr::pull(h1_proxy_specification, "analytical_unit") ==
          "time_slice"
      ),
      msg = "The store does not resolve to one time-slice profile."
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
  # Why: Name the region-by-age unit set of this profile: the common cohort
  #   for the common window, all records for the native window.
  targets::tar_target(
    name = "h1_proxy_slice_set",
    command = dplyr::if_else(
      dplyr::pull(h1_proxy_specification, "age_window") == "common_cohort",
      h1_proxy_config[["common_slice_set"]],
      h1_proxy_config[["native_slice_set"]]
    )
  ),
  # Why: Import the region-by-age units of this profile's window and
  #   regions.
  targets::tar_target(
    name = "data_h1_proxy_units",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_slice_units",
        runner = runner_inputs
      ) |>
        dplyr::filter(
          .data[["slice_set"]] == h1_proxy_slice_set,
          .data[["age"]] >=
            dplyr::pull(h1_proxy_specification, "age_min_bp"),
          .data[["age"]] <=
            dplyr::pull(h1_proxy_specification, "age_max_bp"),
          .data[["region"]] %in% (region_subset %||% .data[["region"]]),
          check_h1_proxy_specification
        )
    }
  ),
  # Why: Import the climate block selected once for each of these units.
  targets::tar_target(
    name = "data_h1_proxy_climate",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_slice_climate",
        runner = runner_inputs
      ) |>
        dplyr::semi_join(
          data_h1_proxy_units,
          by = c("slice_set", "region", "age")
        )
    }
  ),
  # Why: Attach this profile's human block to the shared climate block of
  #   every unit, removing records that lack the proxy at that age.
  targets::tar_target(
    name = "data_h1_proxy_designs",
    command = prepare_shared_climate_hvarpart_designs(
      data_units = data_h1_proxy_units |>
        dplyr::select(dplyr::all_of(c("region", "age", "data_merge"))),
      data_climate_selection = data_h1_proxy_climate |>
        dplyr::select(-dplyr::all_of("slice_set")),
      model_specifications = h1_proxy_specification,
      unit_columns = c("region", "age"),
      drop_rows_without_human = TRUE,
      max_cor = h1_proxy_config[["max_cor"]],
      max_vif = h1_proxy_config[["max_vif"]]
    )
  ),
  # Why: Fit one spatially controlled HVarPart model per unit with the
  #   unchanged group fitter.
  targets::tar_target(
    name = "output_h1_proxy_fits",
    command = fit_local_spatial_hvarpart_models(
      data_designs = data_h1_proxy_designs,
      response_vars = h1_proxy_response_variables,
      seed = h1_proxy_config[["seed"]],
      permutations = h1_proxy_config[["permutations"]],
      alpha = h1_proxy_config[["alpha"]],
      min_unique_locations = h1_proxy_config[["min_unique_locations"]],
      min_residual_df = h1_proxy_config[["min_spatial_residual_df"]],
      distance_km = h1_proxy_config[["spatial_distances_km"]]
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
  # Why: Extract status, dbMEM selection, contribution, and diagnostic
  #   tables.
  targets::tar_target(
    name = "result_h1_proxy_space_control",
    command = {
      check_h1_proxy_model_errors

      summarise_local_spatial_hvarpart_models(
        data_results = output_h1_proxy_fits
      )
    }
  ),
  # Why: Publish an explicit status for every unit of the run.
  targets::tar_target(
    name = "table_h1_proxy_status",
    command = result_h1_proxy_space_control[["status"]] |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Convert contributions to zero-truncated human, climate, and space
  #   compositions per unit.
  targets::tar_target(
    name = "data_h1_proxy_compositions",
    command = prepare_spatial_hvarpart_composition(
      data_components = result_h1_proxy_space_control[["components"]] |>
        dplyr::select(-dplyr::all_of("model_id")),
      data_status = result_h1_proxy_space_control[["status"]] |>
        dplyr::select(-dplyr::all_of("model_id"))
    ) |>
      dplyr::mutate(profile_id = profile_id_h1_proxy, .before = 1L)
  ),
  # Why: Publish the estimable unit compositions used by the decision metric
  #   and by cross-proxy comparisons.
  targets::tar_target(
    name = "data_h1_proxy_estimable_units",
    command = filter_estimable_spatial_compositions(
      data_composition = data_h1_proxy_compositions
    )
  ),
  # Why: Publish the input units of the run for the identical-units check.
  targets::tar_target(
    name = "data_h1_proxy_run_units",
    command = data_h1_proxy_designs |>
      dplyr::transmute(
        profile_id = profile_id_h1_proxy,
        slice_set = h1_proxy_slice_set,
        region = .data[["region"]],
        age = .data[["age"]]
      )
  ),
  # Why: Publish the climate block each unit used, for the shared-climate
  #   check across runs.
  targets::tar_target(
    name = "table_h1_proxy_climate_used",
    command = data_h1_proxy_designs |>
      dplyr::transmute(
        profile_id = profile_id_h1_proxy,
        slice_set = h1_proxy_slice_set,
        region = .data[["region"]],
        age = .data[["age"]],
        climate_set = .data[["climate_set"]],
        selection_status = .data[["selection_status"]],
        n_rows_without_human = .data[["n_rows_without_human"]]
      )
  ),
  # Why: Pool the human-minus-climate allocation over estimable units with
  #   a block-bootstrap interval for each profile (decision metric).
  targets::tar_target(
    name = "output_h1_proxy_decision_metric",
    command = summarise_human_proxy_temporal_decision_metric(
      data_composition = data_h1_proxy_estimable_units,
      block_length = h1_proxy_config[["temporal_block_length"]],
      n_bootstrap = h1_proxy_config[["n_bootstrap"]],
      seed = h1_proxy_config[["seed"]]
    )
  ),
  # Why: Publish the decision metric with the run identifiers.
  targets::tar_target(
    name = "table_h1_proxy_decision_metric",
    command = output_h1_proxy_decision_metric[["metric"]] |>
      dplyr::mutate(
        profile_id = profile_id_h1_proxy,
        analysis = "temporal_h1",
        human_proxy = dplyr::pull(h1_proxy_specification, "human_proxy"),
        proxy_support = dplyr::pull(h1_proxy_specification, "proxy_support"),
        age_window = dplyr::pull(h1_proxy_specification, "age_window"),
        window_id = dplyr::pull(h1_proxy_specification, "window_id"),
        .before = 1L
      )
  ),
  # Why: Export every run table to the run's own output folder.
  targets::tar_target(
    name = "files_h1_proxy_tables",
    command = {
      list_tables <-
        list(
          human_proxy__model_status__space_control = table_h1_proxy_status,
          human_proxy__dbmem_selection__space_control =
            result_h1_proxy_space_control[["selection"]],
          human_proxy__hierarchical_contributions__space_control =
            result_h1_proxy_space_control[["components"]],
          human_proxy__unique_adjusted_r2__space_control =
            result_h1_proxy_space_control[["unique_adjusted_r2"]],
          human_proxy__zero_truncated_composition__space_control =
            data_h1_proxy_compositions,
          human_proxy__climate_selection__space_control =
            table_h1_proxy_climate_used,
          human_proxy__decision_metric__space_control =
            table_h1_proxy_decision_metric,
          human_proxy__decision_units__space_control =
            output_h1_proxy_decision_metric[["units"]]
        )

      vec_paths <-
        file.path(path_tables, stringr::str_c(names(list_tables), ".csv")) |>
        rlang::set_names(names(list_tables))

      save_collinearity_managed_hvarpart_tables(list_tables, vec_paths)
    },
    format = "file"
  )
)
