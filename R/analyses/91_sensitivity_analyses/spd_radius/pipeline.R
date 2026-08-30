#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               SPD radius H1 sensitivity
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines one profile-driven target graph for strict 250 km and 500 km SPD H1
# operations.
# Run with:
#   R/analyses/91_sensitivity_analyses/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"
runner_h1 <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"
runner_sensitivity <-
  "R/analyses/91_sensitivity_analyses/00_run.R"

store_predictors <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/predictors"
  )
store_spd_radius <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/spd"
  )
store_paps <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/paps"
  )
store_h1_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/inputs"
  )
store_canonical_time <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/time_control/spd"
  )
store_canonical_space <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/spatial_control/spd"
  )
store_canonical_aggregation <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "analyses_h1",
      "spatial_aggregation",
      "spd_human_climate_balance",
      sep = "/"
    )
  )
path_profiles <-
  here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint matched SPD coverage and provenance for exported evidence.
  targets::tar_target(
    name = "fingerprint_spd_radius_products",
    command = compute_target_store_fingerprint(
      store = store_spd_radius,
      target_names = c(
        "table_spd_radius_coverage",
        "table_spd_radius_provenance"
      ),
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint the strict-radius predictor products and their invariance
  #   audit before fitting sensitivity models.
  targets::tar_target(
    name = "fingerprint_spd_radius_predictors",
    command = compute_target_store_fingerprint(
      store = store_predictors,
      target_names = c(
        "data_predictors_spd_radius_filtered",
        "table_predictors_spd_radius_invariance"
      ),
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint canonical filtered PAP inputs without copying them.
  targets::tar_target(
    name = "fingerprint_spd_radius_paps",
    command = compute_target_store_fingerprint(
      store = store_paps,
      target_names = "data_properties_filtered",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint shared H1 metadata, response, predictor, and fitting
  #   configuration contracts.
  targets::tar_target(
    name = "fingerprint_spd_radius_h1_inputs",
    command = compute_target_store_fingerprint(
      store = store_h1_inputs,
      target_names = c(
        "data_meta",
        "h1_response_variables",
        "h1_predictor_sets",
        "h1_analysis_config"
      ),
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint canonical fallback outputs imported as contextual
  #   references only.
  targets::tar_target(
    name = "fingerprint_spd_radius_canonical_time",
    command = compute_target_store_fingerprint(
      store = store_canonical_time,
      target_names = "data_time_controlled_balance_records_all",
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint the canonical region-age ranking reference independently.
  targets::tar_target(
    name = "fingerprint_spd_radius_canonical_space",
    command = compute_target_store_fingerprint(
      store = store_canonical_space,
      target_names = c(
        "table_spatial_control_status",
        "table_spatial_control_rankings"
      ),
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint the canonical spatial aggregation reference independently.
  targets::tar_target(
    name = "fingerprint_spd_radius_canonical_aggregation",
    command = compute_target_store_fingerprint(
      store = store_canonical_aggregation,
      target_names = "table_spatiotemporal_balance_estimates",
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track the declarative scenario registry as a file target.
  targets::tar_target(
    name = "file_spd_radius_analysis_profiles",
    command = path_profiles,
    format = "file"
  ),
  # Why: Load only enabled SPD-radius sensitivity profiles.
  targets::tar_target(
    name = "data_spd_radius_analysis_profiles",
    command = load_analysis_profiles(file_spd_radius_analysis_profiles) |>
      dplyr::filter(
        .data[["human_proxy"]] == "spd",
        .data[["profile_role"]] == "sensitivity",
        .data[["configuration_reference"]] == "spd_radius",
        .data[["enabled"]]
      )
  ),
  # Why: Load the matched product's geographic coverage and source hashes for
  #   sensitivity evidence exports.
  targets::tar_target(
    name = "table_spd_radius_coverage",
    command = {
      fingerprint_spd_radius_products

      load_target_store_value(
        store = store_spd_radius,
        target_name = "table_spd_radius_coverage",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Load matched SPD source hashes for exported provenance.
  targets::tar_target(
    name = "table_spd_radius_provenance",
    command = {
      fingerprint_spd_radius_products

      load_target_store_value(
        store = store_spd_radius,
        target_name = "table_spd_radius_provenance",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Load strict-radius predictors after confirming their formal upstream
  #   fingerprint.
  targets::tar_target(
    name = "data_predictors_spd_radius_filtered",
    command = {
      fingerprint_spd_radius_predictors

      load_target_store_value(
        store = store_predictors,
        target_name = "data_predictors_spd_radius_filtered",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Load the canonical filtered PAP data unchanged.
  targets::tar_target(
    name = "data_properties_filtered",
    command = {
      fingerprint_spd_radius_paps

      load_target_store_value(
        store = store_paps,
        target_name = "data_properties_filtered",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Load shared H1 metadata and configuration after one formal
  #   fingerprint invalidates all dependent branches.
  targets::tar_target(
    name = "data_meta",
    command = {
      fingerprint_spd_radius_h1_inputs

      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_meta",
        runner = runner_h1
      )
    }
  ),
  # Why: Load the canonical H1 response-variable contract unchanged.
  targets::tar_target(
    name = "h1_response_variables",
    command = {
      fingerprint_spd_radius_h1_inputs

      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_response_variables",
        runner = runner_h1
      )
    }
  ),
  # Why: Load the canonical H1 SPD predictor groups unchanged.
  targets::tar_target(
    name = "h1_predictor_sets",
    command = {
      fingerprint_spd_radius_h1_inputs

      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_predictor_sets",
        runner = runner_h1
      )
    }
  ),
  # Why: Load the canonical H1 fitting thresholds and seed unchanged.
  targets::tar_target(
    name = "h1_analysis_config",
    command = {
      fingerprint_spd_radius_h1_inputs

      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_analysis_config",
        runner = runner_h1
      )
    }
  ),
  # Why: Split predictors into deterministic radius branches rather than
  #   copying an H1 pipeline for each radius.
  targets::tar_target(
    name = "list_spd_radius_predictor_groups",
    command = split(
      data_predictors_spd_radius_filtered,
      data_predictors_spd_radius_filtered[["radius_km"]]
    ),
    iteration = "list"
  ),
  # Why: Reuse all current H1 operations once per strict-radius branch.
  targets::tar_target(
    name = "output_spd_radius_h1",
    command = run_spd_radius_h1_profile(
      data_predictors_profile = list_spd_radius_predictor_groups,
      data_properties_filtered = data_properties_filtered,
      data_meta = data_meta,
      response_vars = h1_response_variables,
      predictor_vars = h1_predictor_sets[["spd"]],
      analysis_config = h1_analysis_config,
      data_profiles = data_spd_radius_analysis_profiles
    ),
    pattern = map(list_spd_radius_predictor_groups),
    iteration = "list"
  ),
  # Why: Publish the complete temporal-control evidence across both radii.
  targets::tar_target(
    name = "table_spd_radius_dataset_age_collapse",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "dataset_age_collapse"
    )
  ),
  # Why: Publish the complete temporal-control status evidence across radii.
  targets::tar_target(
    name = "table_spd_radius_time_control_status",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "time_control_status"
    )
  ),
  # Why: Publish signed temporal hierarchical contributions for both radii.
  targets::tar_target(
    name = "table_spd_radius_time_control_components",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "time_control_components"
    )
  ),
  # Why: Publish temporal unique adjusted R-squared fractions for both radii.
  targets::tar_target(
    name = "table_spd_radius_time_control_unique_adjusted_r2",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "time_control_unique_adjusted_r2"
    )
  ),
  # Why: Publish temporal residual-dependence diagnostics for both radii.
  targets::tar_target(
    name = "table_spd_radius_time_control_residual_moran",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "time_control_residual_moran"
    )
  ),
  # Why: Publish all time-controlled dataset records before spatial eligibility.
  targets::tar_target(
    name = "data_spd_radius_time_controlled_balance_records_all",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "time_controlled_balance_records_all"
    )
  ),
  # Why: Publish the complete region-age spatial-control evidence.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_status",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_status"
    )
  ),
  # Why: Publish region-age dbMEM selection decisions for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_dbmem_selection",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_selection"
    )
  ),
  # Why: Publish region-age dbMEM network diagnostics for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_dbmem_diagnostics",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_dbmem_diagnostics"
    )
  ),
  # Why: Publish signed region-age hierarchical contributions for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_components",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_components"
    )
  ),
  # Why: Publish zero-truncated region-age compositions for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_composition",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_composition"
    )
  ),
  # Why: Publish region-age human-climate rankings for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_rankings",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_rankings"
    )
  ),
  # Why: Publish region-age unique adjusted R-squared fractions for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_unique_adjusted_r2",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_unique_adjusted_r2"
    )
  ),
  # Why: Publish region-age residual spatial-dependence diagnostics.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_residual_moran",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_residual_moran"
    )
  ),
  # Why: Publish the post-control remaining spatial-signal tests by radius.
  targets::tar_target(
    name = "table_spd_radius_spatial_control_remaining_signal",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatial_control_remaining_signal"
    )
  ),
  # Why: Publish the spatially aggregated balance and dbMEM diagnostics.
  targets::tar_target(
    name = "table_spd_radius_spatiotemporal_balance_estimates",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatiotemporal_balance_estimates"
    )
  ),
  # Why: Publish post-aggregation Moran diagnostics for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatiotemporal_balance_moran",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatiotemporal_balance_moran"
    )
  ),
  # Why: Publish aggregation dbMEM eligibility diagnostics for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatiotemporal_balance_dbmem_diagnostics",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatiotemporal_balance_dbmem_diagnostics"
    )
  ),
  # Why: Publish aggregation dbMEM selections for both radii.
  targets::tar_target(
    name = "table_spd_radius_spatiotemporal_balance_dbmem_selection",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "spatiotemporal_balance_dbmem_selection"
    )
  ),
  # Why: Publish scenario seeds and hashes independently of model tables.
  targets::tar_target(
    name = "table_spd_radius_h1_provenance",
    command = prepare_spd_radius_h1_results(
      output_spd_radius_h1,
      "provenance"
    ) |>
      dplyr::mutate(
        predictor_store_fingerprint =
          fingerprint_spd_radius_predictors,
        paps_store_fingerprint = fingerprint_spd_radius_paps,
        h1_input_store_fingerprint =
          fingerprint_spd_radius_h1_inputs
      )
  ),
  # Why: Assemble one all-available dataset-level source table, retaining
  #   non-estimable statuses before paired filtering.
  targets::tar_target(
    name = "table_spd_radius_spatial_dataset_all_available",
    command = {
      data_unique <-
        table_spd_radius_time_control_unique_adjusted_r2 |>
        dplyr::select(
          dplyr::all_of(c(
            "radius_km",
            "dataset_id",
            "fraction",
            "adjusted_r_squared"
          ))
        ) |>
        tidyr::pivot_wider(
          names_from = "fraction",
          values_from = "adjusted_r_squared",
          names_prefix = "unique_adjusted_r2_"
        )

      table_spd_radius_time_control_status |>
        dplyr::select(
          dplyr::all_of(c("radius_km", "dataset_id", "status"))
        ) |>
        dplyr::left_join(
          data_spd_radius_time_controlled_balance_records_all |>
            dplyr::select(
              dplyr::all_of(c(
                "radius_km",
                "dataset_id",
                "total_adjusted_r_squared",
                "human",
                "climate",
                "time",
                "signed_difference",
                "signed_balance",
                "zero_balance",
                "signed_ranking",
                "zero_ranking"
              ))
            ),
          by = c("radius_km", "dataset_id"),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          data_unique,
          by = c("radius_km", "dataset_id"),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              dplyr::all_of(c(
                "dataset_id",
                "region",
                "climatezone"
              ))
            ) |>
            dplyr::distinct(),
          by = "dataset_id",
          relationship = "many-to-one"
        )
    }
  ),
  # Why: Pair only identical datasets and calculate 500-minus-250 source
  #   differences and qualitative robustness changes.
  targets::tar_target(
    name = "table_spd_radius_spatial_dataset_paired",
    command = prepare_spd_radius_paired_comparison(
      data_source =
        table_spd_radius_spatial_dataset_all_available,
      key_cols = c("dataset_id", "region", "climatezone"),
      balance_cols = c(
        "total_adjusted_r_squared",
        "human",
        "climate",
        "time",
        "signed_difference",
        "signed_balance",
        "zero_balance",
        "unique_adjusted_r2_pure_human",
        "unique_adjusted_r2_pure_climate",
        "unique_adjusted_r2_pure_time"
      ),
      ranking_cols = c("signed_ranking", "zero_ranking"),
      estimable_statuses = c(
        "estimated",
        "estimated_residual_temporal_dependence"
      )
    )
  ),
  # Why: Report matched dataset-level shifts overall and by manuscript
  #   geographic groups without a numeric materiality threshold.
  targets::tar_target(
    name = "table_spd_radius_spatial_dataset_summary",
    command = dplyr::bind_rows(
      summarise_spd_radius_paired_results(
        table_spd_radius_spatial_dataset_paired,
        delta_col = "signed_difference_delta_500_minus_250",
        summary_level = "overall"
      ),
      summarise_spd_radius_paired_results(
        table_spd_radius_spatial_dataset_paired,
        delta_col = "signed_difference_delta_500_minus_250",
        group_cols = "region",
        summary_level = "region"
      ),
      summarise_spd_radius_paired_results(
        table_spd_radius_spatial_dataset_paired,
        delta_col = "signed_difference_delta_500_minus_250",
        group_cols = "climatezone",
        summary_level = "climatezone"
      ),
      summarise_spd_radius_paired_results(
        table_spd_radius_spatial_dataset_paired,
        delta_col = "signed_difference_delta_500_minus_250",
        group_cols = c("region", "climatezone"),
        summary_level = "region_and_climatezone"
      )
    )
  ),
  # Why: Assemble one all-available region-age table with signed,
  #   zero-truncated, and unique adjusted-R-squared source values.
  targets::tar_target(
    name = "table_spd_radius_temporal_region_age_all_available",
    command = {
      data_unique <-
        table_spd_radius_spatial_control_unique_adjusted_r2 |>
        dplyr::select(
          dplyr::all_of(c(
            "radius_km",
            "region",
            "age",
            "fraction",
            "adjusted_r_squared"
          ))
        ) |>
        tidyr::pivot_wider(
          names_from = "fraction",
          values_from = "adjusted_r_squared",
          names_prefix = "unique_adjusted_r2_"
        )
      data_allocations <-
        table_spd_radius_spatial_control_composition |>
        dplyr::select(
          dplyr::all_of(c(
            "radius_km",
            "region",
            "age",
            "predictor",
            "allocation"
          ))
        ) |>
        tidyr::pivot_wider(
          names_from = "predictor",
          values_from = "allocation",
          names_prefix = "zero_allocation_"
        )

      table_spd_radius_spatial_control_status |>
        dplyr::select(
          dplyr::all_of(c(
            "radius_km",
            "region",
            "age",
            "status",
            "selection_status"
          ))
        ) |>
        dplyr::left_join(
          table_spd_radius_spatial_control_rankings |>
            dplyr::select(
              dplyr::all_of(c(
                "radius_km",
                "region",
                "age",
                "human_climate_only_balance",
                "controlled_human",
                "controlled_climate",
                "controlled_balance",
                "human_climate_only_ranking",
                "controlled_ranking"
              ))
            ),
          by = c("radius_km", "region", "age"),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          data_allocations,
          by = c("radius_km", "region", "age"),
          relationship = "one-to-one"
        ) |>
        dplyr::left_join(
          data_unique,
          by = c("radius_km", "region", "age"),
          relationship = "one-to-one"
        )
    }
  ),
  # Why: Pair only identical region-age groups and classify radius-driven
  #   ranking and estimability changes.
  targets::tar_target(
    name = "table_spd_radius_temporal_region_age_paired",
    command = prepare_spd_radius_paired_comparison(
      data_source =
        table_spd_radius_temporal_region_age_all_available,
      key_cols = c("region", "age"),
      balance_cols = c(
        "human_climate_only_balance",
        "controlled_human",
        "controlled_climate",
        "controlled_balance",
        "zero_allocation_human",
        "zero_allocation_climate",
        "zero_allocation_space",
        "unique_adjusted_r2_pure_human",
        "unique_adjusted_r2_pure_climate",
        "unique_adjusted_r2_pure_space"
      ),
      ranking_cols = c(
        "controlled_ranking",
        "human_climate_only_ranking"
      ),
      estimable_statuses = c(
        "spatial_model_estimated",
        "no_spatial_terms_selected"
      )
    )
  ),
  # Why: Report temporal paired shifts overall, by region, and by the exact
  #   region-age units used for exception reporting.
  targets::tar_target(
    name = "table_spd_radius_temporal_region_age_summary",
    command = dplyr::bind_rows(
      summarise_spd_radius_paired_results(
        table_spd_radius_temporal_region_age_paired,
        delta_col = "controlled_balance_delta_500_minus_250",
        summary_level = "overall"
      ),
      summarise_spd_radius_paired_results(
        table_spd_radius_temporal_region_age_paired,
        delta_col = "controlled_balance_delta_500_minus_250",
        group_cols = "region",
        summary_level = "region"
      ),
      summarise_spd_radius_paired_results(
        table_spd_radius_temporal_region_age_paired,
        delta_col = "controlled_balance_delta_500_minus_250",
        group_cols = c("region", "age"),
        summary_level = "region_and_age"
      )
    )
  ),
  # Why: Independently reconstruct every published spatial summary value from
  #   the matched dataset source table.
  targets::tar_target(
    name = "table_spd_radius_spatial_summary_reconciliation",
    command = diagnose_spd_radius_summary_reconciliation(
      data_comparison = table_spd_radius_spatial_dataset_paired,
      data_summary = table_spd_radius_spatial_dataset_summary,
      delta_col = "signed_difference_delta_500_minus_250"
    )
  ),
  # Why: Independently reconstruct every published temporal summary value from
  #   the matched region-age source table.
  targets::tar_target(
    name = "table_spd_radius_temporal_summary_reconciliation",
    command = diagnose_spd_radius_summary_reconciliation(
      data_comparison = table_spd_radius_temporal_region_age_paired,
      data_summary = table_spd_radius_temporal_region_age_summary,
      delta_col = "controlled_balance_delta_500_minus_250"
    )
  ),
  # Why: Collect every qualitative ranking or status change in one reviewer-
  #   traceable table without applying a continuous-effect threshold.
  targets::tar_target(
    name = "table_spd_radius_ranking_status_changes",
    command = dplyr::bind_rows(
      table_spd_radius_spatial_dataset_paired |>
        dplyr::filter(.data[["material_change"]]) |>
        dplyr::transmute(
          analysis_scope = "spatial_dataset",
          analytical_unit = as.character(.data[["dataset_id"]]),
          region = .data[["region"]],
          climatezone = .data[["climatezone"]],
          age = NA_real_,
          status_250_km = .data[["status_250_km"]],
          status_500_km = .data[["status_500_km"]],
          ranking_250_km = .data[["signed_ranking_250_km"]],
          ranking_500_km = .data[["signed_ranking_500_km"]],
          robustness_classification =
            .data[["robustness_classification"]],
          balance_delta_500_minus_250 =
            .data[["signed_difference_delta_500_minus_250"]]
        ),
      table_spd_radius_temporal_region_age_paired |>
        dplyr::filter(.data[["material_change"]]) |>
        dplyr::transmute(
          analysis_scope = "temporal_region_age",
          analytical_unit = stringr::str_c(
            .data[["region"]],
            .data[["age"]],
            sep = "|"
          ),
          region = .data[["region"]],
          climatezone = NA_character_,
          age = .data[["age"]],
          status_250_km = .data[["status_250_km"]],
          status_500_km = .data[["status_500_km"]],
          ranking_250_km = .data[["controlled_ranking_250_km"]],
          ranking_500_km = .data[["controlled_ranking_500_km"]],
          robustness_classification =
            .data[["robustness_classification"]],
          balance_delta_500_minus_250 =
            .data[["controlled_balance_delta_500_minus_250"]]
        )
    )
  ),
  # Why: Build supplement-ready spatial and temporal figures directly from
  #   public sensitivity source tables.
  targets::tar_target(
    name = "figure_spd_radius_spatial_comparison",
    command = plot_spd_radius_spatial_comparison(
      data_all_available =
        table_spd_radius_spatial_dataset_all_available,
      data_summary = table_spd_radius_spatial_dataset_summary
    )
  ),
  # Why: Build the supplement-ready paired region-age comparison figure.
  targets::tar_target(
    name = "figure_spd_radius_temporal_comparison",
    command = plot_spd_radius_temporal_comparison(
      data_all_available =
        table_spd_radius_temporal_region_age_all_available,
      data_paired = table_spd_radius_temporal_region_age_paired
    )
  ),
  # Why: Publish the selected-model human contribution profile as its own
  #   supplement-ready figure rather than a combined patchwork panel.
  targets::tar_target(
    name = "figure_spd_radius_temporal_profiles",
    command = figure_spd_radius_temporal_comparison[["profiles"]]
  ),
  # Why: Publish the paired human-contribution changes separately so small
  #   radius effects remain legible across region rows.
  targets::tar_target(
    name = "figure_spd_radius_temporal_changes",
    command = figure_spd_radius_temporal_comparison[["changes"]]
  ),
  # Why: Export full source values, comparisons, summaries, and provenance to
  #   semantic manuscript evidence paths.
  targets::tar_target(
    name = "files_spd_radius_evidence_tables",
    command = save_spd_radius_evidence_tables(
      data_tables = list(
        coverage = table_spd_radius_coverage,
        spd_provenance = table_spd_radius_provenance,
        spatial_all_available =
          table_spd_radius_spatial_dataset_all_available,
        spatial_paired = table_spd_radius_spatial_dataset_paired,
        spatial_summary = table_spd_radius_spatial_dataset_summary,
        temporal_all_available =
          table_spd_radius_temporal_region_age_all_available,
        temporal_paired = table_spd_radius_temporal_region_age_paired,
        temporal_summary = table_spd_radius_temporal_region_age_summary,
        ranking_status_changes =
          table_spd_radius_ranking_status_changes,
        h1_provenance = table_spd_radius_h1_provenance,
        spatial_reconciliation =
          table_spd_radius_spatial_summary_reconciliation,
        temporal_reconciliation =
          table_spd_radius_temporal_summary_reconciliation
      ),
      file_paths = c(
        coverage = here::here(
          "Outputs/Tables/Data_preparation/SPD",
          "spd__radius_comparison__availability_by_geography.csv"
        ),
        spd_provenance = here::here(
          "Outputs/Tables/Data_preparation/SPD",
          "spd__radius_comparison__source_provenance.csv"
        ),
        spatial_all_available = here::here(
          "Outputs/Tables/H1/Spatial/SPD",
          "spd__radius_comparison__dataset_all_available.csv"
        ),
        spatial_paired = here::here(
          "Outputs/Tables/H1/Spatial/SPD",
          "spd__radius_comparison__dataset_paired.csv"
        ),
        spatial_summary = here::here(
          "Outputs/Tables/H1/Spatial/SPD",
          "spd__radius_comparison__geographic_summary.csv"
        ),
        temporal_all_available = here::here(
          "Outputs/Tables/H1/Temporal/HVarPart",
          "spd__radius_comparison__region_age_all_available.csv"
        ),
        temporal_paired = here::here(
          "Outputs/Tables/H1/Temporal/HVarPart",
          "spd__radius_comparison__region_age_paired.csv"
        ),
        temporal_summary = here::here(
          "Outputs/Tables/H1/Temporal/HVarPart",
          "spd__radius_comparison__region_age_summary.csv"
        ),
        ranking_status_changes = here::here(
          "Outputs/Tables/H1/Spatial/SPD",
          "spd__radius_comparison__ranking_status_changes.csv"
        ),
        h1_provenance = here::here(
          "Outputs/Tables/H1/Spatial/SPD",
          "spd__radius_comparison__h1_provenance.csv"
        ),
        spatial_reconciliation = here::here(
          "Outputs/Tables/H1/Spatial/SPD",
          "spd__radius_comparison__summary_reconciliation.csv"
        ),
        temporal_reconciliation = here::here(
          "Outputs/Tables/H1/Temporal/HVarPart",
          "spd__radius_comparison__summary_reconciliation.csv"
        )
      )
    ),
    format = "file"
  ),
  # Why: Save the spatial and two temporal figures in reviewable raster and
  #   publication-ready vector formats with semantic stems.
  targets::tar_target(
    name = "files_spd_radius_sensitivity_figures",
    command = save_spd_radius_sensitivity_figures(
      plot_spatial = figure_spd_radius_spatial_comparison,
      plot_temporal_profiles = figure_spd_radius_temporal_profiles,
      plot_temporal_changes = figure_spd_radius_temporal_changes,
      path_spatial = here::here(
        "Outputs/Figures/H1/Spatial/SPD",
        "spd__radius_comparison__spatial_human_climate_balance"
      ),
      path_temporal_profiles = here::here(
        "Outputs/Figures/H1/Temporal/HVarPart",
        stringr::str_c(
          "spd__radius_comparison__temporal_human__",
          "untruncated_hierarchical_contribution__space_control"
        )
      ),
      path_temporal_changes = here::here(
        "Outputs/Figures/H1/Temporal/HVarPart",
        stringr::str_c(
          "spd__radius_comparison__temporal_human__",
          "untruncated_hierarchical_contribution_change__space_control"
        )
      )
    ),
    format = "file"
  ),
  # Why: Hash every exported source table and figure for response-document
  #   traceability.
  targets::tar_target(
    name = "table_spd_radius_evidence_manifest",
    command = {
      table_paths <- unname(files_spd_radius_evidence_tables)
      figure_paths <- unname(files_spd_radius_sensitivity_figures)
      table_artifact_ids <-
        paste(
          tolower(basename(dirname(table_paths))),
          tools::file_path_sans_ext(basename(table_paths)),
          sep = "__"
        )

      evidence_index <-
        tibble::tibble(
        artifact_id = c(
          table_artifact_ids,
          "spatial_figure_png",
          "spatial_figure_pdf",
          "temporal_profiles_figure_png",
          "temporal_profiles_figure_pdf",
          "temporal_changes_figure_png",
          "temporal_changes_figure_pdf"
        ),
        description = c(
          paste("Source table", basename(table_paths)),
          "Spatial radius comparison figure PNG",
          "Spatial radius comparison figure PDF",
          "Temporal radius-profile figure PNG",
          "Temporal radius-profile figure PDF",
          "Temporal paired-change figure PNG",
          "Temporal paired-change figure PDF"
        ),
        analysis_profile = "spd_radius",
        source_pipeline =
          "91_sensitivity_analyses/spd_radius/pipeline.R",
        public_target = c(
          rep(
            "files_spd_radius_evidence_tables",
            length(table_paths)
          ),
          rep("files_spd_radius_sensitivity_figures", 6)
        ),
        path = c(table_paths, figure_paths)
      )

      assertthat::assert_that(
        nrow(evidence_index) ==
          length(c(table_paths, figure_paths)),
        !anyDuplicated(evidence_index[["artifact_id"]]),
        msg = "SPD radius evidence index is not one-to-one."
      )

      build_evidence_manifest(evidence_index)
    }
  ),
  # Why: Publish the sensitivity-specific evidence manifest as a stable CSV.
  targets::tar_target(
    name = "file_spd_radius_evidence_manifest",
    command = {
      path_output <- here::here(
        "Outputs/Tables/Reporting",
        "spd__radius_comparison__evidence_manifest.csv"
      )
      dir.create(
        dirname(path_output),
        recursive = TRUE,
        showWarnings = FALSE
      )
      readr::write_csv(table_spd_radius_evidence_manifest, path_output)
      path_output
    },
    format = "file"
  ),
  # Why: Import canonical fallback estimates as a read-only contextual
  #   reference; no sensitivity target writes to canonical H1 stores.
  targets::tar_target(
    name = "data_spd_radius_canonical_time_reference",
    command = {
      fingerprint_spd_radius_canonical_time

      load_target_store_value(
        store = store_canonical_time,
        target_name = "data_time_controlled_balance_records_all",
        runner = runner_h1
      )
    }
  ),
  # Why: Import the canonical region-age ranking reference without mutation.
  targets::tar_target(
    name = "table_spd_radius_canonical_space_reference",
    command = {
      fingerprint_spd_radius_canonical_space

      load_target_store_value(
        store = store_canonical_space,
        target_name = "table_spatial_control_rankings",
        runner = runner_h1
      )
    }
  ),
  # Why: Import the canonical aggregate balance reference without mutation.
  targets::tar_target(
    name = "table_spd_radius_canonical_aggregation_reference",
    command = {
      fingerprint_spd_radius_canonical_aggregation

      load_target_store_value(
        store = store_canonical_aggregation,
        target_name = "table_spatiotemporal_balance_estimates",
        runner = runner_h1
      )
    }
  )
)
