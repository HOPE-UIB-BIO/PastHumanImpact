#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#       Collinearity-managed human-proxy H1 HVarPart
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Run with:
#   R/analyses/91_sensitivity_analyses/human_proxy_hvarpart_collinearity_managed/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Canonical H1 and the unfiltered joint pipeline remain unchanged.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)
source(here::here("R/00_Config_file.R"))

runner_h1 <- "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"
runner_proxy <- "R/analyses/91_sensitivity_analyses/human_proxy_convergence/00_run.R"
runner_joint <- "R/analyses/91_sensitivity_analyses/joint_human_proxy_hvarpart/00_run.R"
store_h1 <- resolve_pipeline_store_path(data_storage_path, "analyses_h1/inputs")
store_proxy <- resolve_pipeline_store_path(
  data_storage_path, "sensitivity_analyses/human_proxy_convergence"
)
store_joint <- resolve_pipeline_store_path(
  data_storage_path, "sensitivity_analyses/joint_human_proxy_hvarpart"
)
path_profiles <- here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")
path_geo <- file.path(data_storage_path, "Spatial", "Climatezones", "data_geo_koppen.rds")
path_figures <- here::here(
  "Outputs", "Figures", "H1", "Sensitivity",
  "Human_proxy_hvarpart_collinearity_managed"
)
path_tables <- here::here(
  "Outputs", "Tables", "H1", "Sensitivity",
  "Human_proxy_hvarpart_collinearity_managed"
)
path_decision_figures <- file.path(path_figures, "Decision_memo_variants")
path_proxy_atlas_figures <- file.path(path_figures, "Proxy_spatial_atlas")
path_proxy_atlas_tables <- file.path(path_tables, "Proxy_spatial_atlas")
path_canonical_spd_balance <- here::here(
  "Outputs", "Tables", "H1", "Spatial", "SPD",
  "spd__human_climate_balance__dataset_values__time_and_space_control.csv"
)
path_canonical_spd_components <- here::here(
  "Outputs", "Tables", "H1", "Spatial", "SPD",
  "spd__human_climate_time__component_profiles__dataset_values__time_control.csv"
)
path_canonical_spd_temporal <- here::here(
  "Outputs", "Tables", "H1", "Temporal", "HVarPart",
  paste0(
    "spd_events__human_climate_space__zero_truncated_hierarchical_",
    "composition__space_control.csv"
  )
)
config <- list(
  age_min = 2000, age_max = 8000, min_unique_ages = 10L,
  min_temporal_residual_df = 4L, max_cor = 0.8, max_vif = 5,
  climate = c("temp_annual", "temp_cold", "prec_summer", "prec_win")
)
profile_ids <- c(
  "within_dataset_collinearity_managed_joint_filtered_time_control",
  "time_slice_collinearity_managed_joint_filtered_spatial_control",
  "within_dataset_collinearity_managed_spd_matched_bridge_time_control",
  "time_slice_collinearity_managed_spd_matched_bridge_spatial_control"
)

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track the canonical H1 inputs without rebuilding their store.
  targets::tar_target(
    name = "fingerprint_colmanaged_h1",
    command =     compute_target_store_fingerprint(
      store_h1,
      c("data_hvar_filtered_unique_age", "data_hvar_timebins_spd_unique_age",
        "data_meta", "h1_response_variables", "h1_analysis_config"),
      runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fail early with actionable acquisition instructions for Issue #342.
  targets::tar_target(
    name = "fingerprint_colmanaged_proxy",
    command =     tryCatch(
      compute_target_store_fingerprint(
        store_proxy, "data_human_proxy_matches", runner_proxy
      ),
      error = function(err) cli::cli_abort(c(
        "Matched SPD, KK10, and HYDE observations are unavailable.",
        "i" = paste(
          "Run: Rscript R/analyses/91_sensitivity_analyses/",
          "human_proxy_convergence/download_sources.R", sep = ""
        ),
        "i" = paste(
          "Then run: Rscript R/analyses/91_sensitivity_analyses/",
          "human_proxy_convergence/00_run.R", sep = ""
        ),
        "x" = conditionMessage(err)
      ))
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track available unfiltered evidence for diagnostic comparison only.
  targets::tar_target(
    name = "fingerprint_colmanaged_unfiltered_joint",
    command =     tryCatch(
      compute_target_store_fingerprint(
        store_joint,
        c("data_joint_time_controlled_balance_records",
          "table_joint_spatial_control_composition"),
        runner_joint
      ),
      error = function(err) NA_character_
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track registry changes as part of the analysis definition.
  targets::tar_target(
    name = "file_colmanaged_profiles",
    command = path_profiles,
    format = "file"
  ),
  # Why: Track the canonical Köppen spatial layer used by Figure 3.
  targets::tar_target(
    name = "file_colmanaged_geo",
    command = path_geo,
    format = "file"
  ),
  # Why: Track the exact canonical SPD source table behind the comparison map.
  targets::tar_target(
    name = "file_colmanaged_canonical_spd_balance",
    command = path_canonical_spd_balance,
    format = "file"
  ),
  # Why: Track canonical per-sequence unique adjusted R-squared components.
  targets::tar_target(
    name = "file_colmanaged_canonical_spd_components",
    command = path_canonical_spd_components,
    format = "file"
  ),
  # Why: Track the canonical SPD spatial-control composition for the memo.
  targets::tar_target(
    name = "file_colmanaged_canonical_spd_temporal",
    command = path_canonical_spd_temporal,
    format = "file"
  ),
  # Why: Import canonical within-dataset response and predictor histories.
  targets::tar_target(
    name = "data_colmanaged_within_source",
    command = {
      force(fingerprint_colmanaged_h1)
      load_target_store_value(
        store_h1, "data_hvar_filtered_unique_age", runner_h1
      )
    }
  ),
  # Why: Import canonical region-by-age response and predictor tables.
  targets::tar_target(
    name = "data_colmanaged_slice_source",
    command = {
      force(fingerprint_colmanaged_h1)
      load_target_store_value(
        store_h1, "data_hvar_timebins_spd_unique_age", runner_h1
      )
    }
  ),
  # Why: Import canonical dataset metadata for maps and region validation.
  targets::tar_target(
    name = "data_colmanaged_metadata",
    command = {
      force(fingerprint_colmanaged_h1)
      load_target_store_value(store_h1, "data_meta", runner_h1)
    }
  ),
  # Why: Import canonical multivariate response columns.
  targets::tar_target(
    name = "colmanaged_response_variables",
    command = {
      force(fingerprint_colmanaged_h1)
      load_target_store_value(store_h1, "h1_response_variables", runner_h1)
    }
  ),
  # Why: Import canonical thresholds, permutations, distances, and seed.
  targets::tar_target(
    name = "colmanaged_h1_config",
    command = {
      force(fingerprint_colmanaged_h1)
      load_target_store_value(store_h1, "h1_analysis_config", runner_h1)
    }
  ),
  # Why: Import the fully matched Issue #342 proxy observations.
  targets::tar_target(
    name = "data_colmanaged_proxy_matches",
    command = {
      force(fingerprint_colmanaged_proxy)
      load_target_store_value(
        store_proxy, "data_human_proxy_matches", runner_proxy
      )
    }
  ),
  # Why: Import unfiltered joint balances for an explicitly diagnostic contrast.
  targets::tar_target(
    name = "data_colmanaged_unfiltered_balance",
    command = {
      force(fingerprint_colmanaged_unfiltered_joint)
      if (is.na(fingerprint_colmanaged_unfiltered_joint)) {
        tibble::tibble()
      } else {
        load_target_store_value(
          store_joint, "data_joint_time_controlled_balance_records", runner_joint
        )
      }
    }
  ),
  # Why: Import unfiltered joint compositions for a diagnostic contrast only.
  targets::tar_target(
    name = "data_colmanaged_unfiltered_composition",
    command = {
      force(fingerprint_colmanaged_unfiltered_joint)
      if (is.na(fingerprint_colmanaged_unfiltered_joint)) {
        tibble::tibble()
      } else {
        load_target_store_value(
          store_joint, "table_joint_spatial_control_composition", runner_joint
        )
      }
    }
  ),
  # Why: Validate the joint and matched-SPD bridge sensitivity profiles.
  targets::tar_target(
    name = "data_colmanaged_profiles",
    command =     load_analysis_profiles(file_colmanaged_profiles) |>
      dplyr::filter(.data[["profile_id"]] %in% profile_ids)
  ),
  # Why: Define the primary joint model and its matched SPD diagnostic bridge.
  targets::tar_target(
    name = "data_colmanaged_model_specs",
    command =     build_human_proxy_model_specifications()
  ),
  # Why: Preserve raw/transformed proxies and enforce matched canonical keys.
  targets::tar_target(
    name = "output_colmanaged_inputs",
    command =     prepare_collinearity_managed_human_proxy_inputs(
      data_colmanaged_within_source, data_colmanaged_slice_source,
      data_colmanaged_proxy_matches, data_colmanaged_metadata,
      config$age_min, config$age_max
    )
  ),
  # Why: Expose the matched 2--8 ka within-dataset input.
  targets::tar_target(
    name = "data_colmanaged_within",
    command = output_colmanaged_inputs$within_dataset
  ),
  # Why: Expose the matched 2--8 ka region-age input.
  targets::tar_target(
    name = "data_colmanaged_slices",
    command = output_colmanaged_inputs$time_slices
  ),
  # Why: Map the exact matched inputs used by the sensitivity analysis.
  targets::tar_target(
    name = "output_colmanaged_proxy_spatial_atlas",
    command = prepare_human_proxy_spatial_atlas_data(
      data_within = output_colmanaged_inputs$within_dataset,
      data_metadata = data_colmanaged_metadata,
      age_min = config$age_min,
      age_max = config$age_max,
      colour_quantile = 0.99
    )
  ),
  # Why: Build one world-and-Europe comparison page per 500-year slice.
  targets::tar_target(
    name = "plots_colmanaged_proxy_spatial_atlas",
    command = {
      atlas_ages <- sort(
        unique(output_colmanaged_proxy_spatial_atlas$values$age),
        decreasing = TRUE
      )
      rlang::set_names(
        purrr::map(
          atlas_ages,
          ~ plot_human_proxy_spatial_atlas_page(
            output_colmanaged_proxy_spatial_atlas$values,
            age = .x
          )
        ),
        as.character(atlas_ages)
      )
    }
  ),
  # Why: Select the local human and climate blocks once per dataset.
  targets::tar_target(
    name = "data_colmanaged_temporal_designs",
    command =     prepare_local_hvarpart_model_designs(
      data_colmanaged_within, data_colmanaged_model_specs, "dataset_id",
      climate_candidates = config$climate,
      max_cor = config$max_cor, max_vif = config$max_vif
    )
  ),
  # Why: Select the local human and climate blocks once per region-age.
  targets::tar_target(
    name = "data_colmanaged_spatial_designs",
    command =     prepare_local_hvarpart_model_designs(
      data_colmanaged_slices, data_colmanaged_model_specs, c("region", "age"),
      climate_candidates = config$climate,
      max_cor = config$max_cor, max_vif = config$max_vif
    )
  ),
  # Why: Export local selection and protected-time collinearity diagnostics.
  targets::tar_target(
    name = "output_colmanaged_temporal_audits",
    command =     summarise_local_predictor_selection_audits(
      data_colmanaged_temporal_designs, "dataset_id", include_time = TRUE
    )
  ),
  # Why: Export local selection and focal spatial-design diagnostics.
  targets::tar_target(
    name = "output_colmanaged_spatial_audits",
    command =     summarise_local_predictor_selection_audits(
      data_colmanaged_spatial_designs, c("region", "age"), include_time = FALSE
    )
  ),
  # Why: Fit time-controlled models through the unchanged single-model fitter.
  targets::tar_target(
    name = "output_colmanaged_temporal_fits",
    command =     fit_local_temporal_hvarpart_models(
      data_colmanaged_temporal_designs, colmanaged_response_variables,
      seed = colmanaged_h1_config$seed,
      min_unique_ages = config$min_unique_ages,
      min_residual_df = config$min_temporal_residual_df,
      distance_years = colmanaged_h1_config$temporal_distances_years,
      permutations = colmanaged_h1_config$permutations
    )
  ),
  # Why: Fit spatially controlled models through the unchanged group fitter.
  targets::tar_target(
    name = "output_colmanaged_spatial_fits",
    command =     fit_local_spatial_hvarpart_models(
      data_colmanaged_spatial_designs, colmanaged_response_variables,
      seed = colmanaged_h1_config$seed,
      permutations = colmanaged_h1_config$permutations,
      alpha = colmanaged_h1_config$alpha,
      min_unique_locations = colmanaged_h1_config$min_unique_locations,
      min_residual_df = colmanaged_h1_config$min_spatial_residual_df,
      distance_km = colmanaged_h1_config$spatial_distances_km
    )
  ),
  # Why: Require every caught fit error to be visible and halt on current errors.
  targets::tar_target(
    name = "check_colmanaged_model_errors",
    command =     {
      errors <- dplyr::bind_rows(
        output_colmanaged_temporal_fits |>
          dplyr::filter(!is.na(.data[["error_message"]])) |>
          dplyr::transmute(
            path = "time_control",
            unit = .data[["dataset_id"]],
            model_id = .data[["model_id"]],
            error_message = .data[["error_message"]]
          ),
        output_colmanaged_spatial_fits |>
          dplyr::filter(!is.na(.data[["error_message"]])) |>
          dplyr::transmute(
            path = "spatial_control",
            unit = paste(.data[["region"]], .data[["age"]]),
            model_id = .data[["model_id"]],
            error_message = .data[["error_message"]]
          )
      )
      if (nrow(errors) > 0L) cli::cli_abort(c(
        "Unclassified HVarPart model errors remain.",
        "x" = paste(utils::capture.output(print(errors, n = Inf)), collapse = "\n")
      ))
      TRUE
    }
  ),
  # Why: Extract canonical temporal result tables for every model.
  targets::tar_target(
    name = "result_colmanaged_temporal",
    command =     {check_colmanaged_model_errors; summarise_local_temporal_hvarpart_models(output_colmanaged_temporal_fits)}
  ),
  # Why: Extract canonical spatial result tables for every model.
  targets::tar_target(
    name = "result_colmanaged_spatial",
    command =     {check_colmanaged_model_errors; summarise_local_spatial_hvarpart_models(output_colmanaged_spatial_fits)}
  ),
  # Why: Diagnose full final spatial designs including protected selected dbMEMs.
  targets::tar_target(
    name = "output_colmanaged_spatial_control_diagnostics",
    command = summarise_spatial_control_collinearity_diagnostics(
      output_colmanaged_spatial_fits,
      colmanaged_response_variables
    )
  ),
  # Why: Build eligible source tables for the single filtered joint model.
  targets::tar_target(
    name = "output_colmanaged_comparisons",
    command =     prepare_human_proxy_comparison_tables(
      result_colmanaged_temporal, result_colmanaged_spatial,
      data_colmanaged_metadata
    )
  ),
  # Why: Prepare like-for-like matched SPD evidence using the same climate sets.
  targets::tar_target(
    name = "output_colmanaged_bridge_comparisons",
    command = prepare_human_proxy_comparison_tables(
      result_colmanaged_temporal, result_colmanaged_spatial,
      data_colmanaged_metadata, model_id = "spd_matched_bridge"
    )
  ),
  # Why: Prove climate selection is exactly shared across bridge and joint fits.
  targets::tar_target(
    name = "table_colmanaged_shared_climate_selection",
    command = dplyr::bind_rows(
      validate_shared_local_climate_selections(
        data_colmanaged_temporal_designs, "dataset_id"
      ) |>
        dplyr::mutate(analysis_path = "time_control", .before = 1L),
      validate_shared_local_climate_selections(
        data_colmanaged_spatial_designs, c("region", "age")
      ) |>
        dplyr::mutate(analysis_path = "spatial_control", .before = 1L)
    )
  ),
  # Why: Pair every eligible sequence with its canonical SPD-only result and
  # expose local predictor-selection differences by geography.
  targets::tar_target(
    name = "output_colmanaged_sequence_overview",
    command = prepare_sequence_r2_predictor_overview(
      canonical_balance = readr::read_csv(
        file_colmanaged_canonical_spd_balance, show_col_types = FALSE
      ),
      canonical_components = readr::read_csv(
        file_colmanaged_canonical_spd_components, show_col_types = FALSE
      ),
      filtered_balance = output_colmanaged_comparisons$balance_common,
      filtered_unique_r2 = result_colmanaged_temporal$unique_adjusted_r2 |>
        dplyr::filter(.data[["model_id"]] == "joint_filtered"),
      predictor_selection = output_colmanaged_temporal_audits$selection |>
        dplyr::filter(.data[["model_id"]] == "joint_filtered")
    )
  ),
  # Why: Isolate interval/cohort changes from expansion of the human block.
  targets::tar_target(
    name = "output_colmanaged_decision_memo_evidence",
    command = prepare_human_proxy_decision_memo_evidence(
      canonical_balance = readr::read_csv(
        file_colmanaged_canonical_spd_balance, show_col_types = FALSE
      ),
      canonical_components = readr::read_csv(
        file_colmanaged_canonical_spd_components, show_col_types = FALSE
      ),
      canonical_temporal = readr::read_csv(
        file_colmanaged_canonical_spd_temporal, show_col_types = FALSE
      ),
      bridge_comparisons = output_colmanaged_bridge_comparisons,
      joint_comparisons = output_colmanaged_comparisons,
      filtered_temporal_unique = result_colmanaged_temporal$unique_adjusted_r2,
      age_min = config$age_min,
      age_max = config$age_max
    )
  ),
  # Why: Express every decision-memo spatial variant in the canonical schema.
  targets::tar_target(
    name = "data_colmanaged_decision_spatial_records",
    command = output_colmanaged_decision_memo_evidence$spatial_plot_records
  ),
  # Why: Spatially adjust all three Figure-3 variants on one exact cohort.
  targets::tar_target(
    name = "output_colmanaged_decision_balance_aggregations",
    command = fit_human_proxy_spatial_balance_models(
      data_colmanaged_decision_spatial_records,
      seed = colmanaged_h1_config$seed,
      permutations = colmanaged_h1_config$permutations,
      alpha = colmanaged_h1_config$alpha,
      min_unique_locations = colmanaged_h1_config$min_unique_locations,
      min_residual_df = colmanaged_h1_config$min_spatial_residual_df,
      distance_km = colmanaged_h1_config$spatial_distances_km
    )
  ),
  # Why: Reconcile the locked canonical matched-input coverage expectations.
  targets::tar_target(
    name = "table_colmanaged_input_coverage",
    command = dplyr::bind_rows(
      output_colmanaged_inputs$within_dataset |>
        tidyr::unnest(cols = dplyr::all_of("data_merge")) |>
        dplyr::summarise(
          input = "within_dataset", rows = dplyr::n(),
          datasets = dplyr::n_distinct(.data[["dataset_id"]]),
          ages = dplyr::n_distinct(.data[["age"]]), groups = NA_integer_
        ),
      output_colmanaged_inputs$time_slices |>
        dplyr::summarise(
          input = "region_age", rows = sum(.data[["n_samples"]]),
          datasets = dplyr::n_distinct(unlist(purrr::map(
            .data[["data_merge"]], "dataset_id"
          ))),
          ages = dplyr::n_distinct(.data[["age"]]),
          groups = dplyr::n()
        )
    )
  ),
  # Why: Export filtered-versus-unfiltered joint contrasts.
  targets::tar_target(
    name = "output_colmanaged_contrasts",
    command = prepare_human_proxy_model_contrasts(
      output_colmanaged_comparisons$balance_common,
      output_colmanaged_comparisons$composition_common,
      data_colmanaged_unfiltered_balance,
      data_colmanaged_unfiltered_composition
    )
  ),
  # Why: Aggregate eligible joint-model balances using canonical spatial control.
  targets::tar_target(
    name = "output_colmanaged_balance_aggregations",
    command =     fit_human_proxy_spatial_balance_models(
      output_colmanaged_comparisons$balance_common,
      seed = colmanaged_h1_config$seed,
      permutations = colmanaged_h1_config$permutations,
      alpha = colmanaged_h1_config$alpha,
      min_unique_locations = colmanaged_h1_config$min_unique_locations,
      min_residual_df = colmanaged_h1_config$min_spatial_residual_df,
      distance_km = colmanaged_h1_config$spatial_distances_km
    )
  ),
  # Why: Prepare the canonical Köppen-derived region plotting layer.
  targets::tar_target(
    name = "data_colmanaged_geo",
    command =     readr::read_rds(file_colmanaged_geo) |>
      tibble::as_tibble() |>
      dplyr::mutate(climatezone = dplyr::case_when(
        .data[["ecozone_koppen_15"]] == "Cold_Without_dry_season" ~ .data[["ecozone_koppen_30"]],
        .data[["ecozone_koppen_5"]] %in% c("Cold", "Temperate") ~ .data[["ecozone_koppen_15"]],
        .default = .data[["ecozone_koppen_5"]]
      )) |>
      prepare_climatezone_factor()
  ),
  # Why: Build the Figure-3-style filtered joint spatial result.
  targets::tar_target(
    name = "plot_colmanaged_spatial",
    command =     plot_human_proxy_spatial_balance_comparison(
      output_colmanaged_comparisons$balance_common,
      output_colmanaged_balance_aggregations,
      data_colmanaged_geo,
      data_colmanaged_model_specs |>
        dplyr::filter(.data[["model_id"]] == "joint_filtered")
    )
  ),
  # Why: Build the Figure-4-style filtered joint temporal result.
  targets::tar_target(
    name = "plot_colmanaged_temporal",
    command =     plot_human_proxy_temporal_comparison(
      output_colmanaged_comparisons$composition_common,
      data_colmanaged_model_specs |>
        dplyr::filter(.data[["model_id"]] == "joint_filtered"),
      config$age_min, config$age_max
    )
  ),
  # Why: Reproduce the Figure-3 layout for canonical SPD on the common cohort.
  targets::tar_target(
    name = "plot_colmanaged_decision_spatial_canonical",
    command = plot_human_proxy_spatial_balance_comparison(
      data_colmanaged_decision_spatial_records,
      output_colmanaged_decision_balance_aggregations,
      data_colmanaged_geo,
      tibble::tibble(model_id = "canonical_spd")
    )
  ),
  # Why: Reproduce the Figure-3 layout for the matched SPD bridge.
  targets::tar_target(
    name = "plot_colmanaged_decision_spatial_bridge",
    command = plot_human_proxy_spatial_balance_comparison(
      data_colmanaged_decision_spatial_records,
      output_colmanaged_decision_balance_aggregations,
      data_colmanaged_geo,
      tibble::tibble(model_id = "spd_matched_bridge")
    )
  ),
  # Why: Reproduce the Figure-3 layout for the joint model on the same cohort.
  targets::tar_target(
    name = "plot_colmanaged_decision_spatial_joint",
    command = plot_human_proxy_spatial_balance_comparison(
      data_colmanaged_decision_spatial_records,
      output_colmanaged_decision_balance_aggregations,
      data_colmanaged_geo,
      tibble::tibble(model_id = "joint_filtered")
    )
  ),
  # Why: Reproduce the Figure-4 layout for canonical SPD on the common cohort.
  targets::tar_target(
    name = "plot_colmanaged_decision_temporal_canonical",
    command = plot_h1_temporal_joint_human_proxy_composition(
      output_colmanaged_decision_memo_evidence$temporal_common_cohort |>
        dplyr::filter(.data[["model_id"]] == "canonical_spd") |>
        dplyr::transmute(
          analysis = .data[["model_id"]],
          region = .data[["continental_region"]],
          age = .data[["age"]], predictor = .data[["predictor"]],
          allocation = .data[["allocation"]]
        ),
      human_label = "Human"
    )
  ),
  # Why: Reproduce the Figure-4 layout for the matched SPD bridge.
  targets::tar_target(
    name = "plot_colmanaged_decision_temporal_bridge",
    command = plot_h1_temporal_joint_human_proxy_composition(
      output_colmanaged_decision_memo_evidence$temporal_common_cohort |>
        dplyr::filter(.data[["model_id"]] == "spd_matched_bridge") |>
        dplyr::transmute(
          analysis = .data[["model_id"]],
          region = .data[["continental_region"]],
          age = .data[["age"]], predictor = .data[["predictor"]],
          allocation = .data[["allocation"]]
        ),
      human_label = "Human"
    )
  ),
  # Why: Reproduce the Figure-4 layout for the filtered joint model.
  targets::tar_target(
    name = "plot_colmanaged_decision_temporal_joint",
    command = plot_h1_temporal_joint_human_proxy_composition(
      output_colmanaged_decision_memo_evidence$temporal_common_cohort |>
        dplyr::filter(.data[["model_id"]] == "joint_filtered") |>
        dplyr::transmute(
          analysis = .data[["model_id"]],
          region = .data[["continental_region"]],
          age = .data[["age"]], predictor = .data[["predictor"]],
          allocation = .data[["allocation"]]
        ),
      human_label = "Human"
    )
  ),
  # Why: Put the three Figure-4 variants on one readable landscape panel.
  targets::tar_target(
    name = "plot_colmanaged_decision_temporal_comparison",
    command = {
      shared_legend <- cowplot::get_legend(
        plot_colmanaged_decision_temporal_joint +
          ggplot2::theme(legend.position = "bottom")
      )
      comparison_panels <- cowplot::plot_grid(
        plot_colmanaged_decision_temporal_canonical +
          ggplot2::theme(legend.position = "none"),
        plot_colmanaged_decision_temporal_bridge +
          ggplot2::theme(legend.position = "none"),
        plot_colmanaged_decision_temporal_joint +
          ggplot2::theme(legend.position = "none"),
        nrow = 1L,
        labels = c(
          "A  Canonical SPD",
          "B  Matched SPD bridge",
          "C  Filtered joint human block"
        ),
        label_size = text_size * 0.9,
        label_x = 0.01,
        hjust = 0
      )
      cowplot::plot_grid(
        comparison_panels,
        shared_legend,
        ncol = 1L,
        rel_heights = c(1, 0.08)
      )
    }
  ),
  # Why: Show per-sequence adjusted R-squared changes, not only aggregate maps.
  targets::tar_target(
    name = "plot_colmanaged_sequence_r2",
    command = plot_sequence_r2_comparison(
      comparison_long = output_colmanaged_sequence_overview$comparison_long,
      r2_summary = output_colmanaged_sequence_overview$r2_summary
    )
  ),
  # Why: Make local human and climate predictor retention visible by geography.
  targets::tar_target(
    name = "plot_colmanaged_predictor_selection",
    command = plot_predictor_selection_overview(
      selection_frequency_by_continental_region =
        output_colmanaged_sequence_overview[[
          "selection_frequency_by_continental_region"
        ]],
      selection_frequency_by_region =
        output_colmanaged_sequence_overview[[
          "selection_frequency_by_region"
        ]]
    )
  ),
  # Why: Save exactly the two primary joint-model figures in PNG and PDF.
  targets::tar_target(
    name = "files_colmanaged_hvarpart_figures",
    command = save_joint_human_proxy_hvarpart_figures(
      plot_colmanaged_spatial, plot_colmanaged_temporal,
      file.path(path_figures, paste0(
        "collinearity_filtered_joint_human_proxies__human_climate_balance__",
        "zero_truncated__time_and_space_control"
      )),
      file.path(path_figures, paste0(
        "collinearity_filtered_joint_human_proxies__human_climate_space__",
        "zero_truncated_hierarchical_composition__space_control"
      ))
    ),
    format = "file"
  ),
  # Why: Save exact-cohort Figure-3 and Figure-4 variants for the memo.
  targets::tar_target(
    name = "files_colmanaged_decision_variant_figures",
    command = unlist(c(
      save_joint_human_proxy_hvarpart_figures(
        plot_colmanaged_decision_spatial_canonical,
        plot_colmanaged_decision_temporal_canonical,
        file.path(path_decision_figures, "fig3_canonical_spd_common"),
        file.path(path_decision_figures, "fig4_canonical_spd_common")
      ),
      save_joint_human_proxy_hvarpart_figures(
        plot_colmanaged_decision_spatial_bridge,
        plot_colmanaged_decision_temporal_bridge,
        file.path(path_decision_figures, "fig3_matched_spd_bridge_common"),
        file.path(path_decision_figures, "fig4_matched_spd_bridge_common")
      ),
      save_joint_human_proxy_hvarpart_figures(
        plot_colmanaged_decision_spatial_joint,
        plot_colmanaged_decision_temporal_joint,
        file.path(path_decision_figures, "fig3_filtered_joint_common"),
        file.path(path_decision_figures, "fig4_filtered_joint_common")
      )
    ), use.names = FALSE),
    format = "file"
  ),
  # Why: Export one landscape Figure-4 comparison for the co-author memo.
  targets::tar_target(
    name = "files_colmanaged_decision_temporal_comparison",
    command = {
      force(files_colmanaged_decision_variant_figures)
      save_diagnostic_figure_formats(
        fig_object = plot_colmanaged_decision_temporal_comparison,
        fig_name = "fig4_three_model_comparison_common",
        path_figures = path_decision_figures,
        width = 270,
        height = 175,
        units = "mm"
      )
    },
    format = "file"
  ),
  # Why: Export one PNG per age and a 13-page companion PDF atlas.
  targets::tar_target(
    name = "files_colmanaged_proxy_spatial_atlas",
    command = save_human_proxy_spatial_atlas(
      plot_pages = plots_colmanaged_proxy_spatial_atlas,
      output_directory = path_proxy_atlas_figures
    ),
    format = "file"
  ),
  # Why: Export the exact plotted values, fixed scales, and age coverage.
  targets::tar_target(
    name = "files_colmanaged_proxy_spatial_atlas_tables",
    command = {
      dir.create(
        path_proxy_atlas_tables,
        recursive = TRUE,
        showWarnings = FALSE
      )
      atlas_tables <- list(
        matched_proxy_spatial_atlas_values =
          output_colmanaged_proxy_spatial_atlas$values,
        matched_proxy_spatial_atlas_scales =
          output_colmanaged_proxy_spatial_atlas$scales,
        matched_proxy_spatial_atlas_coverage =
          output_colmanaged_proxy_spatial_atlas$coverage
      )
      atlas_paths <- file.path(
        path_proxy_atlas_tables,
        paste0(names(atlas_tables), ".csv")
      )
      purrr::walk2(atlas_tables, atlas_paths, readr::write_csv)
      atlas_paths
    },
    format = "file"
  ),
  # Why: Save supporting sequence-level diagnostics outside the two primary
  # figure paths, in both raster and vector formats.
  targets::tar_target(
    name = "files_colmanaged_sequence_overview_figures",
    command = save_collinearity_managed_overview_figures(
      plot_r2 = plot_colmanaged_sequence_r2,
      plot_selection = plot_colmanaged_predictor_selection,
      path_r2 = file.path(
        path_figures, "Diagnostics",
        "sequence_adjusted_r2__filtered_joint_vs_spd_only"
      ),
      path_selection = file.path(
        path_figures, "Diagnostics",
        "predictor_selection__continental_region_and_region"
      )
    ),
    format = "file"
  ),
  # Why: Quantify source-variable skewness without choosing a transformation.
  targets::tar_target(
    name = "table_colmanaged_proxy_provenance",
    command =     output_colmanaged_inputs$provenance |>
      dplyr::left_join(
        output_colmanaged_inputs$matched_values |>
          dplyr::select(dplyr::all_of(c("spd_raw", "spd_sqrt", "kk10_fraction", "hyde_raw", "hyde_sqrt"))) |>
          tidyr::pivot_longer(dplyr::everything(), names_to = "variable", values_to = "value") |>
          dplyr::summarise(
            n = dplyr::n(), min = min(.data[["value"]]), median = stats::median(.data[["value"]]),
            max = max(.data[["value"]]),
            skewness = mean((.data[["value"]] - mean(.data[["value"]]))^3) / stats::sd(.data[["value"]])^3,
            .by = "variable"
          ), by = "variable"
      )
  ),
  # Why: Export selection, diagnostics, eligibility, fits, and exact plot sources.
  targets::tar_target(
    name = "files_colmanaged_hvarpart_tables",
    command =     {
      tables <- list(
        proxy_provenance = table_colmanaged_proxy_provenance,
        input_coverage = table_colmanaged_input_coverage,
        temporal_selection = output_colmanaged_temporal_audits$selection,
        spatial_selection = output_colmanaged_spatial_audits$selection,
        temporal_selection_frequencies = output_colmanaged_temporal_audits$selection_frequencies,
        spatial_selection_frequencies = output_colmanaged_spatial_audits$selection_frequencies,
        temporal_correlations = output_colmanaged_temporal_audits$correlations,
        spatial_correlations = output_colmanaged_spatial_audits$correlations,
        temporal_vif = output_colmanaged_temporal_audits$vif,
        spatial_vif = output_colmanaged_spatial_audits$vif,
        temporal_condition_indices = output_colmanaged_temporal_audits$condition_indices,
        spatial_condition_indices = output_colmanaged_spatial_audits$condition_indices,
        temporal_design = output_colmanaged_temporal_audits$design,
        spatial_design = output_colmanaged_spatial_audits$design,
        spatial_full_control_correlations = output_colmanaged_spatial_control_diagnostics$correlations,
        spatial_full_control_vif = output_colmanaged_spatial_control_diagnostics$vif,
        spatial_full_control_condition_indices = output_colmanaged_spatial_control_diagnostics$condition_indices,
        spatial_full_control_design = output_colmanaged_spatial_control_diagnostics$design,
        temporal_status = result_colmanaged_temporal$status,
        temporal_components = result_colmanaged_temporal$components,
        temporal_unique_adjusted_r2 = result_colmanaged_temporal$unique_adjusted_r2,
        temporal_residual_moran = result_colmanaged_temporal$residual_moran,
        spatial_status = result_colmanaged_spatial$status,
        spatial_dbmem_selection = result_colmanaged_spatial$selection,
        spatial_dbmem_diagnostics = result_colmanaged_spatial$dbmem_diagnostics,
        spatial_components = result_colmanaged_spatial$components,
        spatial_unique_adjusted_r2 = result_colmanaged_spatial$unique_adjusted_r2,
        spatial_residual_moran = result_colmanaged_spatial$residual_moran,
        spatial_remaining_signal = result_colmanaged_spatial$remaining_spatial_test,
        shared_climate_selection = table_colmanaged_shared_climate_selection,
        temporal_eligibility = output_colmanaged_comparisons$temporal_eligibility,
        spatial_eligibility = output_colmanaged_comparisons$spatial_eligibility,
        bridge_temporal_eligibility =
          output_colmanaged_bridge_comparisons$temporal_eligibility,
        bridge_spatial_eligibility =
          output_colmanaged_bridge_comparisons$spatial_eligibility,
        bridge_all_available_spatial_balances =
          output_colmanaged_bridge_comparisons$balance_all_available,
        bridge_plotted_spatial_balances =
          output_colmanaged_bridge_comparisons$balance_common,
        bridge_plotted_temporal_compositions =
          output_colmanaged_bridge_comparisons$composition_common,
        sequence_r2_comparison =
          output_colmanaged_sequence_overview$sequence_values,
        sequence_r2_summary =
          output_colmanaged_sequence_overview$r2_summary,
        predictor_selection_by_dataset =
          output_colmanaged_sequence_overview$selection_by_dataset,
        predictor_selection_frequency_by_continental_region =
          output_colmanaged_sequence_overview[[
            "selection_frequency_by_continental_region"
          ]],
        predictor_selection_frequency_by_region =
          output_colmanaged_sequence_overview[[
            "selection_frequency_by_region"
          ]],
        predictor_selection_sets_by_continental_region =
          output_colmanaged_sequence_overview[[
            "selection_sets_by_continental_region"
          ]],
        predictor_selection_sets_by_region =
          output_colmanaged_sequence_overview[[
            "selection_sets_by_region"
          ]],
        all_available_spatial_balances = output_colmanaged_comparisons$balance_all_available,
        plotted_spatial_balances = output_colmanaged_comparisons$balance_common,
        plotted_temporal_compositions = output_colmanaged_comparisons$composition_common,
        decision_spatial_common_cohort =
          output_colmanaged_decision_memo_evidence$spatial_common_cohort,
        decision_spatial_summary =
          output_colmanaged_decision_memo_evidence$spatial_summary,
        decision_spatial_transitions =
          output_colmanaged_decision_memo_evidence$spatial_transitions,
        decision_temporal_common_cohort =
          output_colmanaged_decision_memo_evidence$temporal_common_cohort,
        decision_temporal_summary =
          output_colmanaged_decision_memo_evidence$temporal_summary,
        decision_temporal_transitions =
          output_colmanaged_decision_memo_evidence$temporal_transitions,
        filtered_unfiltered_joint_balance_differences = output_colmanaged_contrasts$filtered_unfiltered_joint_balance,
        filtered_unfiltered_joint_composition_differences = output_colmanaged_contrasts$filtered_unfiltered_joint_composition
      )
      paths <- file.path(path_tables, paste0(names(tables), ".csv"))
      names(paths) <- names(tables)
      save_collinearity_managed_hvarpart_tables(tables, paths)
    },
    format = "file"
  ),
  # Why: Retain the old unfiltered evidence under an explicit diagnostic label.
  targets::tar_target(
    name = "files_colmanaged_unfiltered_joint_reference",
    command =     save_unfiltered_joint_reference(
      c(
        here::here("Outputs", "Figures", "H1", "Sensitivity", "Joint_human_proxy_hvarpart"),
        here::here("Outputs", "Tables", "H1", "Sensitivity", "Joint_human_proxy_hvarpart")
      ),
      file.path(path_figures, "Unfiltered_joint_reference")
    ),
    format = "file"
  )
)
