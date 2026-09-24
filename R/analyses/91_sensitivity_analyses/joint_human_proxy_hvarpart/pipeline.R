#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#         Joint SPD, KK10, and HYDE H1 HVarPart
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines an optional sensitivity target graph.
# Run with:
#   R/analyses/91_sensitivity_analyses/joint_human_proxy_hvarpart/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Canonical H1 stores and outputs are never modified.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_h1 <- "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"
runner_proxy <-
  "R/analyses/91_sensitivity_analyses/human_proxy_convergence/00_run.R"
runner_proxy_download <-
  paste(
    "Rscript",
    paste0(
      "R/analyses/91_sensitivity_analyses/human_proxy_convergence/",
      "download_sources.R"
    )
  )
runner_proxy_analysis <- stringr::str_c("Rscript ", runner_proxy)

store_h1_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )
store_human_proxy <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "sensitivity_analyses/human_proxy_convergence"
  )

path_profiles <-
  here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")
path_geo_koppen <-
  file.path(data_storage_path, "Spatial", "Climatezones", "data_geo_koppen.rds")
path_tables <-
  here::here(
    "Outputs",
    "Tables",
    "H1",
    "Sensitivity",
    "Joint_human_proxy_hvarpart"
  )
path_figures <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Sensitivity",
    "Joint_human_proxy_hvarpart"
  )

joint_config <-
  list(
    age_min = 2000,
    age_max = 8000,
    min_unique_ages = 10L,
    min_temporal_residual_df = 4L,
    analysis_time_control = "spatial_joint_human_proxies",
    analysis_spatial_control = "temporal_joint_human_proxies",
    human_predictors =
      c("spd_transformed", "kk10_transformed", "hyde_transformed")
  )

joint_profile_ids <-
  c(
    "within_dataset_joint_human_proxies_time_control",
    "time_slice_joint_human_proxies_spatial_control"
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "fingerprint_joint_h1_inputs",
    compute_target_store_fingerprint(
      store = store_h1_inputs,
      target_names = c(
        "data_hvar_filtered_unique_age",
        "data_hvar_timebins_spd_unique_age",
        "data_meta",
        "h1_response_variables",
        "h1_predictor_sets",
        "h1_analysis_config"
      ),
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "fingerprint_joint_human_proxy",
    tryCatch(
      compute_target_store_fingerprint(
        store = store_human_proxy,
        target_names = "data_human_proxy_matches",
        runner = runner_proxy
      ),
      error = function(err) {
        cli::cli_abort(
          c(
            "Matched SPD, KK10, and HYDE inputs are unavailable.",
            "i" = "Acquire missing sources with: {.code {runner_proxy_download}}",
            "i" = "Then build the matched target with: {.code {runner_proxy_analysis}}",
            "x" = conditionMessage(err)
          )
        )
      }
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "file_joint_analysis_profiles",
    path_profiles,
    format = "file"
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "file_joint_geo_koppen",
    path_geo_koppen,
    format = "file"
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_within_source",
    {
      fingerprint_joint_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_hvar_filtered_unique_age",
        runner = runner_h1
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_time_slice_source",
    {
      fingerprint_joint_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_hvar_timebins_spd_unique_age",
        runner = runner_h1
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_metadata",
    {
      fingerprint_joint_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_meta",
        runner = runner_h1
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "joint_response_variables",
    {
      fingerprint_joint_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_response_variables",
        runner = runner_h1
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "joint_canonical_predictor_sets",
    {
      fingerprint_joint_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_predictor_sets",
        runner = runner_h1
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "joint_h1_config",
    {
      fingerprint_joint_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_analysis_config",
        runner = runner_h1
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_human_proxy_matches",
    {
      fingerprint_joint_human_proxy
      load_target_store_value(
        store = store_human_proxy,
        target_name = "data_human_proxy_matches",
        runner = runner_proxy
      )
    }
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_profiles",
    load_analysis_profiles(file_joint_analysis_profiles) |>
      dplyr::filter(.data[["profile_id"]] %in% joint_profile_ids)
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "joint_predictor_vars",
    list(
      human = joint_config[["human_predictors"]],
      climate = joint_canonical_predictor_sets[["spd"]][["climate"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "output_joint_human_proxy_inputs",
    prepare_joint_human_proxy_hvarpart_inputs(
      data_within_dataset = data_joint_within_source,
      data_time_slices = data_joint_time_slice_source,
      data_proxy_matches = data_joint_human_proxy_matches,
      data_metadata = data_joint_metadata,
      age_min = joint_config[["age_min"]],
      age_max = joint_config[["age_max"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_within_dataset",
    output_joint_human_proxy_inputs[["within_dataset"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_time_slices",
    output_joint_human_proxy_inputs[["time_slices"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "output_joint_input_diagnostics",
    diagnose_joint_human_proxy_hvarpart_inputs(
      data_within_dataset = data_joint_within_dataset,
      data_time_slices = data_joint_time_slices,
      response_vars = joint_response_variables,
      predictor_vars = joint_predictor_vars,
      min_unique_ages = joint_config[["min_unique_ages"]],
      min_temporal_residual_df =
        joint_config[["min_temporal_residual_df"]],
      min_spatial_residual_df = joint_h1_config[["min_spatial_residual_df"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_within_design",
    output_joint_input_diagnostics[["within_dataset"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_design",
    output_joint_input_diagnostics[["time_slices"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_input_coverage",
    dplyr::bind_rows(
      output_joint_human_proxy_inputs[["matched_values"]] |>
        dplyr::summarise(
          input = "matched_proxy_product",
          rows = dplyr::n(),
          datasets = dplyr::n_distinct(.data[["dataset_id"]]),
          ages = dplyr::n_distinct(.data[["age"]]),
          groups = NA_integer_
        ),
      data_joint_within_dataset |>
        tidyr::unnest(cols = dplyr::all_of("data_merge")) |>
        dplyr::summarise(
          input = "within_dataset_join",
          rows = dplyr::n(),
          datasets = dplyr::n_distinct(.data[["dataset_id"]]),
          ages = dplyr::n_distinct(.data[["age"]]),
          groups = NA_integer_
        ),
      data_joint_time_slices |>
        dplyr::summarise(
          input = "region_age_join",
          rows = sum(.data[["n_samples"]]),
          datasets = dplyr::n_distinct(
            unlist(purrr::map(.data[["data_merge"]], "dataset_id"))
          ),
          ages = dplyr::n_distinct(.data[["age"]]),
          groups = dplyr::n()
        )
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_provenance",
    tibble::tibble(
      field = c(
        "age_min_bp",
        "age_max_bp",
        "human_predictors",
        "climate_predictors",
        "spd_radius",
        "temporal_min_unique_ages",
        "temporal_min_residual_df",
        "scaling",
        "h1_input_fingerprint",
        "proxy_input_fingerprint"
      ),
      value = c(
        as.character(joint_config[["age_min"]]),
        as.character(joint_config[["age_max"]]),
        stringr::str_c(joint_predictor_vars[["human"]], collapse = ";"),
        stringr::str_c(joint_predictor_vars[["climate"]], collapse = ";"),
        "250_km_with_500_km_fallback",
        as.character(joint_config[["min_unique_ages"]]),
        as.character(joint_config[["min_temporal_residual_df"]]),
        "predictor groups scaled by fit_varhp",
        fingerprint_joint_h1_inputs,
        fingerprint_joint_human_proxy
      )
    )
  ),

  # Figure 3 analogue: time control followed by spatial aggregation.
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "output_joint_time_control",
    fit_temporal_hvarpart_datasets(
      data_source = data_joint_within_dataset,
      response_vars = joint_response_variables,
      predictor_vars = joint_predictor_vars,
      min_unique_ages = joint_config[["min_unique_ages"]],
      min_residual_df = joint_config[["min_temporal_residual_df"]],
      distance_years = joint_h1_config[["temporal_distances_years"]],
      permutations = joint_h1_config[["permutations"]],
      seed = joint_h1_config[["seed"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "result_joint_time_control",
    summarise_temporal_hvarpart_results(
      data_results = output_joint_time_control,
      analysis = joint_config[["analysis_time_control"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_time_control_status",
    result_joint_time_control[["status"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_time_control_components",
    result_joint_time_control[["components"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_time_control_unique_adjusted_r2",
    result_joint_time_control[["unique_adjusted_r2"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_time_control_residual_moran",
    result_joint_time_control[["residual_moran"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_time_controlled_balance_records_all",
    prepare_time_controlled_importance_records(
      data_components = table_joint_time_control_components,
      data_status = table_joint_time_control_status,
      data_meta = data_joint_metadata
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_time_controlled_balance_records",
    data_joint_time_controlled_balance_records_all |>
      dplyr::filter(
        is.finite(.data[["signed_difference"]]),
        .data[["signed_weight"]] > 0,
        is.finite(.data[["zero_balance"]]),
        .data[["zero_weight"]] > 0
      )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "output_joint_spatial_balance",
    fit_spatial_importance(
      data_records = data_joint_time_controlled_balance_records,
      permutations = joint_h1_config[["permutations"]],
      alpha = joint_h1_config[["alpha"]],
      min_unique_locations = joint_h1_config[["min_unique_locations"]],
      min_residual_df = joint_h1_config[["min_spatial_residual_df"]],
      distance_km = joint_h1_config[["spatial_distances_km"]],
      seed = joint_h1_config[["seed"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_balance_estimates",
    output_joint_spatial_balance[["estimates"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_balance_moran",
    output_joint_spatial_balance[["moran_diagnostics"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_balance_dbmem_diagnostics",
    output_joint_spatial_balance[["dbmem"]][["diagnostics"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_balance_dbmem_selection",
    tibble::tibble(
      status = output_joint_spatial_balance[["selection"]][["status"]],
      n_complete = output_joint_spatial_balance[["selection"]][["n_complete"]],
      n_candidates = output_joint_spatial_balance[["selection"]][["n_candidates"]],
      global_p_value = output_joint_spatial_balance[["selection"]][["global_p_value"]],
      full_adjusted_r_squared =
        output_joint_spatial_balance[["selection"]][["full_adjusted_r_squared"]],
      selected_terms = stringr::str_c(
        output_joint_spatial_balance[["selection"]][["selected_names"]],
        collapse = ";"
      )
    )
  ),

  # Figure 4 analogue: spatial control within every region-age group.
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "output_joint_spatial_control",
    fit_spatial_hvarpart_dataset(
      data_source = data_joint_time_slices,
      analysis = joint_config[["analysis_spatial_control"]],
      response_vars = joint_response_variables,
      predictor_vars = joint_predictor_vars,
      permutations = joint_h1_config[["permutations"]],
      alpha = joint_h1_config[["alpha"]],
      min_unique_locations = joint_h1_config[["min_unique_locations"]],
      min_residual_df = joint_h1_config[["min_spatial_residual_df"]],
      distance_km = joint_h1_config[["spatial_distances_km"]],
      seed = joint_h1_config[["seed"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "result_joint_spatial_control",
    summarise_spatial_hvarpart_results(output_joint_spatial_control)
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_status",
    result_joint_spatial_control[["status"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_selection",
    result_joint_spatial_control[["selection"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_dbmem_diagnostics",
    result_joint_spatial_control[["dbmem_diagnostics"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_components",
    result_joint_spatial_control[["components"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_unique_adjusted_r2",
    result_joint_spatial_control[["unique_adjusted_r2"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_residual_moran",
    result_joint_spatial_control[["residual_moran"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_remaining_signal",
    result_joint_spatial_control[["remaining_spatial_test"]]
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_spatial_control_composition",
    prepare_spatial_hvarpart_composition(
      data_components = table_joint_spatial_control_components,
      data_status = table_joint_spatial_control_status
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "table_joint_h1_result_records",
    dplyr::bind_rows(
      prepare_h1_result_records(
        data_components = table_joint_time_control_components,
        data_status = table_joint_time_control_status,
        profile_id = joint_profile_ids[[1]],
        model_specification = "human_climate_time",
        proxy = "spd_kk10_hyde",
        analytical_unit = "within_dataset",
        selected_control_dimensions = "time",
        input_hash = stringr::str_c(
          fingerprint_joint_h1_inputs,
          fingerprint_joint_human_proxy,
          sep = "|"
        ),
        profile_hash = rlang::hash(
          data_joint_profiles |>
            dplyr::filter(.data[["profile_id"]] == joint_profile_ids[[1]])
        ),
        configuration_hash = rlang::hash(list(joint_h1_config, joint_config))
      ),
      prepare_h1_result_records(
        data_components = table_joint_spatial_control_components,
        data_status = table_joint_spatial_control_status,
        profile_id = joint_profile_ids[[2]],
        model_specification = "human_climate_space",
        proxy = "spd_kk10_hyde",
        analytical_unit = "time_slice",
        selected_control_dimensions = "space",
        input_hash = stringr::str_c(
          fingerprint_joint_h1_inputs,
          fingerprint_joint_human_proxy,
          sep = "|"
        ),
        profile_hash = rlang::hash(
          data_joint_profiles |>
            dplyr::filter(.data[["profile_id"]] == joint_profile_ids[[2]])
        ),
        configuration_hash = rlang::hash(list(joint_h1_config, joint_config))
      )
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "data_joint_geo_koppen",
    readr::read_rds(file_joint_geo_koppen) |>
      tibble::as_tibble() |>
      dplyr::mutate(
        climatezone = dplyr::case_when(
          .data[["ecozone_koppen_15"]] == "Cold_Without_dry_season" ~
            .data[["ecozone_koppen_30"]],
          .data[["ecozone_koppen_5"]] %in% c("Cold", "Temperate") ~
            .data[["ecozone_koppen_15"]],
          .default = .data[["ecozone_koppen_5"]]
        )
      ) |>
      prepare_climatezone_factor()
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "plot_joint_spatial_balance",
    plot_h1_spatial_controlled_balance(
      data_records = data_joint_time_controlled_balance_records,
      data_estimates = table_joint_spatial_balance_estimates,
      data_geo_koppen = data_joint_geo_koppen,
      profile = "zero_truncated"
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "plot_joint_temporal_composition",
    plot_h1_temporal_joint_human_proxy_composition(
      data_stack = table_joint_spatial_control_composition,
      age_min = joint_config[["age_min"]],
      age_max = joint_config[["age_max"]]
    )
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "files_joint_human_proxy_hvarpart_figures",
    save_joint_human_proxy_hvarpart_figures(
      plot_spatial = plot_joint_spatial_balance,
      plot_temporal = plot_joint_temporal_composition,
      path_spatial = file.path(
        path_figures,
        paste0(
          "joint_human_proxies__human_climate_balance__",
          "zero_truncated__time_and_space_control"
        )
      ),
      path_temporal = file.path(
        path_figures,
        paste0(
          "joint_human_proxies__human_climate_space__",
          "zero_truncated_hierarchical_composition__space_control"
        )
      )
    ),
    format = "file"
  ),
  # Why: Materialize this dependency or result reproducibly.
  targets::tar_target(
    name = "files_joint_human_proxy_hvarpart_tables",
    save_joint_human_proxy_hvarpart_tables(
      data_tables = list(
        input_coverage = table_joint_input_coverage,
        provenance = table_joint_provenance,
        within_design = table_joint_within_design,
        spatial_design = table_joint_spatial_design,
        time_control_status = table_joint_time_control_status,
        time_control_components = table_joint_time_control_components,
        time_control_unique_adjusted_r2 =
          table_joint_time_control_unique_adjusted_r2,
        time_control_residual_moran =
          table_joint_time_control_residual_moran,
        spatial_balance_records = data_joint_time_controlled_balance_records,
        spatial_balance_estimates = table_joint_spatial_balance_estimates,
        spatial_balance_moran = table_joint_spatial_balance_moran,
        spatial_balance_dbmem_diagnostics =
          table_joint_spatial_balance_dbmem_diagnostics,
        spatial_balance_dbmem_selection =
          table_joint_spatial_balance_dbmem_selection,
        spatial_control_status = table_joint_spatial_control_status,
        spatial_control_selection = table_joint_spatial_control_selection,
        spatial_control_dbmem_diagnostics =
          table_joint_spatial_control_dbmem_diagnostics,
        spatial_control_components = table_joint_spatial_control_components,
        spatial_control_unique_adjusted_r2 =
          table_joint_spatial_control_unique_adjusted_r2,
        spatial_control_residual_moran =
          table_joint_spatial_control_residual_moran,
        spatial_control_remaining_signal =
          table_joint_spatial_control_remaining_signal,
        spatial_control_composition =
          table_joint_spatial_control_composition,
        h1_result_records = table_joint_h1_result_records
      ),
      file_paths = c(
        input_coverage = file.path(path_tables, "input_coverage.csv"),
        provenance = file.path(path_tables, "provenance.csv"),
        within_design = file.path(path_tables, "within_design.csv"),
        spatial_design = file.path(path_tables, "spatial_design.csv"),
        time_control_status = file.path(path_tables, "time_control_status.csv"),
        time_control_components =
          file.path(path_tables, "time_control_components.csv"),
        time_control_unique_adjusted_r2 =
          file.path(path_tables, "time_control_unique_adjusted_r2.csv"),
        time_control_residual_moran =
          file.path(path_tables, "time_control_residual_moran.csv"),
        spatial_balance_records =
          file.path(path_tables, "spatial_balance_records.csv"),
        spatial_balance_estimates =
          file.path(path_tables, "spatial_balance_estimates.csv"),
        spatial_balance_moran =
          file.path(path_tables, "spatial_balance_moran.csv"),
        spatial_balance_dbmem_diagnostics =
          file.path(path_tables, "spatial_balance_dbmem_diagnostics.csv"),
        spatial_balance_dbmem_selection =
          file.path(path_tables, "spatial_balance_dbmem_selection.csv"),
        spatial_control_status =
          file.path(path_tables, "spatial_control_status.csv"),
        spatial_control_selection =
          file.path(path_tables, "spatial_control_selection.csv"),
        spatial_control_dbmem_diagnostics =
          file.path(path_tables, "spatial_control_dbmem_diagnostics.csv"),
        spatial_control_components =
          file.path(path_tables, "spatial_control_components.csv"),
        spatial_control_unique_adjusted_r2 =
          file.path(path_tables, "spatial_control_unique_adjusted_r2.csv"),
        spatial_control_residual_moran =
          file.path(path_tables, "spatial_control_residual_moran.csv"),
        spatial_control_remaining_signal =
          file.path(path_tables, "spatial_control_remaining_signal.csv"),
        spatial_control_composition =
          file.path(path_tables, "spatial_control_composition.csv"),
        h1_result_records = file.path(path_tables, "h1_result_records.csv")
      )
    ),
    format = "file"
  )
)
