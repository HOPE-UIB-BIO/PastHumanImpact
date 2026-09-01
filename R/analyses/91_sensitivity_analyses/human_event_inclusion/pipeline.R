#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#          Region-specific human-event sensitivity
#
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines the cohort- and region-aware H1 event-inclusion target graph.
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

store_predictors <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/predictors"
  )
store_events <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/events"
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
path_profiles <-
  here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint canonical predictors without modifying their store.
  targets::tar_target(
    name = "fingerprint_human_event_predictors",
    command = compute_target_store_fingerprint(
      store = store_predictors,
      target_names = "data_predictors_filtered",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint chronology availability independently from predictors.
  targets::tar_target(
    name = "fingerprint_human_event_chronologies",
    command = compute_target_store_fingerprint(
      store = store_events,
      target_names = "events_temporal_subset",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint filtered PAP responses without copying upstream targets.
  targets::tar_target(
    name = "fingerprint_human_event_paps",
    command = compute_target_store_fingerprint(
      store = store_paps,
      target_names = "data_properties_filtered",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Fingerprint shared metadata and H1 fitting contracts once.
  targets::tar_target(
    name = "fingerprint_human_event_h1_inputs",
    command = compute_target_store_fingerprint(
      store = store_h1_inputs,
      target_names = c(
        "data_meta",
        "h1_response_variables",
        "h1_analysis_config"
      ),
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track the full canonical time-bin input separately so adding the
  #   younger event-only extension does not invalidate the six core fits.
  targets::tar_target(
    name = "fingerprint_human_event_full_timebins",
    command = compute_target_store_fingerprint(
      store = store_h1_inputs,
      target_names = "data_hvar_timebins_unique_age",
      runner = runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track all 18 declarative profiles as a file dependency.
  targets::tar_target(
    name = "file_human_event_analysis_profiles",
    command = path_profiles,
    format = "file"
  ),
  # Why: Restrict execution to enabled profiles for these two cohorts.
  targets::tar_target(
    name = "data_human_event_analysis_profiles",
    command = load_analysis_profiles(file_human_event_analysis_profiles) |>
      dplyr::filter(
        .data[["configuration_reference"]] %in% c(
          "human_event_inclusion_as_coded",
          "human_event_inclusion_observed_only"
        ),
        .data[["enabled"]]
      )
  ),
  # Why: Import canonical filtered predictors as a read-only input.
  targets::tar_target(
    name = "data_human_event_predictors",
    command = {
      fingerprint_human_event_predictors
      load_target_store_value(
        store = store_predictors,
        target_name = "data_predictors_filtered",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Import have_events before cohort construction so defaults remain
  #   distinguishable from observed chronologies.
  targets::tar_target(
    name = "data_human_event_chronologies",
    command = {
      fingerprint_human_event_chronologies
      load_target_store_value(
        store = store_events,
        target_name = "events_temporal_subset",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Import filtered PAP trajectories unchanged across all variants.
  targets::tar_target(
    name = "data_human_event_properties",
    command = {
      fingerprint_human_event_paps
      load_target_store_value(
        store = store_paps,
        target_name = "data_properties_filtered",
        runner = runner_data_preparation
      )
    }
  ),
  # Why: Import continent metadata used by every regional resolver call.
  targets::tar_target(
    name = "data_human_event_meta",
    command = {
      fingerprint_human_event_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_meta",
        runner = runner_h1
      )
    }
  ),
  # Why: Keep PAP response variables identical across every scenario.
  targets::tar_target(
    name = "human_event_response_variables",
    command = {
      fingerprint_human_event_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_response_variables",
        runner = runner_h1
      )
    }
  ),
  # Why: Keep fitting thresholds and structural-control settings identical.
  targets::tar_target(
    name = "human_event_analysis_config",
    command = {
      fingerprint_human_event_h1_inputs
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "h1_analysis_config",
        runner = runner_h1
      )
    }
  ),
  # Why: Import the full 0.5--8.5 ka continent-age input for the event-only
  #   trajectory beyond the 2 ka limit of the SPD comparison.
  targets::tar_target(
    name = "data_human_event_full_timebins",
    command = {
      fingerprint_human_event_full_timebins
      load_target_store_value(
        store = store_h1_inputs,
        target_name = "data_hvar_timebins_unique_age",
        runner = runner_h1
      )
    }
  ),
  # Why: Construct the two cohorts and expose chronology/model provenance.
  targets::tar_target(
    name = "output_human_event_inputs",
    command = prepare_human_event_sensitivity_inputs(
      data_predictors = data_human_event_predictors,
      events_temporal_subset = data_human_event_chronologies,
      data_meta = data_human_event_meta
    )
  ),
  # Why: Publish all 1,270 chronology records and regional predictor choices.
  targets::tar_target(
    name = "table_human_event_model_audit",
    command = output_human_event_inputs[["model_audit"]]
  ),
  # Why: Assert the reviewer-facing 1,270/1,024/246 availability contract.
  targets::tar_target(
    name = "table_human_event_availability",
    command = {
      result <- output_human_event_inputs[["availability_summary"]]
      expected <- c(
        "default_filled" = 246L,
        "observed_events" = 1024L,
        "total" = 1270L
      )
      observed <- stats::setNames(
        result[["n_datasets"]],
        result[["chronology_availability"]]
      )
      if (!identical(observed[names(expected)], expected)) {
        cli::cli_abort("Event chronology availability has changed.")
      }
      result
    }
  ),
  # Why: Publish an event-source manifest with observed/default cohort counts.
  targets::tar_target(
    name = "table_human_event_event_manifest",
    command = table_human_event_availability |>
      dplyr::mutate(
        source_target = "events_temporal_subset",
        source_pipeline = "01_data_preparation/03_archaeological_proxies/events/pipeline.R",
        .before = 1L
      )
  ),
  # Why: Publish one explicit regional predictor contract per proxy variant.
  targets::tar_target(
    name = "table_human_event_predictor_manifest",
    command = table_human_event_model_audit |>
      dplyr::distinct(
        .data[["region"]],
        .data[["proxy_variant"]],
        .data[["requested_regional_events"]],
        .data[["retained_human_predictors"]],
        .data[["retained_climate_predictors"]],
        .data[["excluded_event_variables"]],
        .data[["reference_category"]]
      )
  ),
  # Why: Cross the two cohorts with the three proxy variants deterministically.
  targets::tar_target(
    name = "list_human_event_scenarios",
    command = {
      cohorts <- output_human_event_inputs[["cohorts"]]
      tidyr::crossing(
        cohort_index = seq_len(nrow(cohorts)),
        proxy_variant = c("spd", "spd_events", "events")
      ) |>
        dplyr::mutate(
          scenario = purrr::map2(
            .data[["cohort_index"]],
            .data[["proxy_variant"]],
            ~ list(
              cohort = cohorts[["cohort"]][[.x]],
              proxy_variant = .y,
              data_predictors = cohorts[["data_predictors"]][[.x]]
            )
          )
        ) |>
        dplyr::pull(.data[["scenario"]])
    },
    iteration = "list"
  ),
  # Why: Run region-aware time, space, and aggregation controls per scenario.
  targets::tar_target(
    name = "output_human_event_h1",
    command = run_human_event_h1_profile(
      data_predictors_cohort =
        list_human_event_scenarios[["data_predictors"]],
      cohort = list_human_event_scenarios[["cohort"]],
      proxy_variant = list_human_event_scenarios[["proxy_variant"]],
      data_properties_filtered = data_human_event_properties,
      data_meta = data_human_event_meta,
      response_vars = human_event_response_variables,
      analysis_config = human_event_analysis_config,
      data_profiles = data_human_event_analysis_profiles
    ),
    pattern = map(list_human_event_scenarios),
    iteration = "list"
  ),
  # Why: Export the complete within-dataset status evidence for every scenario.
  targets::tar_target(
    name = "table_human_event_time_control_status",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "time_control_status"
    )
  ),
  # Why: Export untruncated within-dataset H1 components for every scenario.
  targets::tar_target(
    name = "table_human_event_time_control_components",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "time_control_components"
    )
  ),
  # Why: Export unique adjusted-R-squared fractions for within-dataset fits.
  targets::tar_target(
    name = "table_human_event_time_control_unique_adjusted_r2",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "time_control_unique_adjusted_r2"
    )
  ),
  # Why: Export signed and zero-truncated dataset balance records.
  targets::tar_target(
    name = "table_human_event_time_balance_all_available",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "time_controlled_balance_records_all"
    )
  ),
  # Why: Export complete continent-age status and estimability evidence.
  targets::tar_target(
    name = "table_human_event_spatial_control_status",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "spatial_control_status"
    )
  ),
  # Why: Export untruncated continent-age H1 components.
  targets::tar_target(
    name = "table_human_event_spatial_control_components",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "spatial_control_components"
    )
  ),
  # Why: Export continent-age unique adjusted-R-squared fractions.
  targets::tar_target(
    name = "table_human_event_spatial_control_unique_adjusted_r2",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "spatial_control_unique_adjusted_r2"
    )
  ),
  # Why: Export signed, zero-truncated, and ranking continent-age evidence.
  targets::tar_target(
    name = "table_human_event_spatial_control_rankings",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "spatial_control_rankings"
    )
  ),
  # Why: Export zero-truncated continent-age allocations for matched evidence.
  targets::tar_target(
    name = "table_human_event_spatial_control_composition",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "spatial_control_composition"
    )
  ),
  # Why: Export spatial-aggregation estimates with scenario provenance.
  targets::tar_target(
    name = "table_human_event_spatiotemporal_estimates",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "spatiotemporal_balance_estimates"
    )
  ),
  # Why: Match spatially aggregated units within cohort and geography.
  targets::tar_target(
    name = "table_human_event_spatiotemporal_matched",
    command = prepare_human_event_matched_comparison(
      data_source = table_human_event_spatiotemporal_estimates |>
        dplyr::mutate(status = .data[["selection_status"]]),
      key_cols = c(
        "aggregation_level",
        "profile",
        "region",
        "climatezone"
      ),
      metric_cols = c("adjusted_balance", "n_records", "weight_sum"),
      ranking_cols = "ranking",
      estimable_statuses = c("selected", "no_spatial_signal")
    )
  ),
  # Why: Summarise aggregate balance contrasts by profile and spatial level.
  targets::tar_target(
    name = "table_human_event_spatiotemporal_summary",
    command = summarise_human_event_matched_comparison(
      data_comparison = table_human_event_spatiotemporal_matched,
      metric_cols = "adjusted_balance",
      group_cols = c("aggregation_level", "profile"),
      summary_level = "spatial_aggregation"
    )
  ),
  # Why: Export fit hashes, profile IDs, and seeds for all six scenarios.
  targets::tar_target(
    name = "table_human_event_h1_provenance",
    command = prepare_human_event_h1_results(
      output_human_event_h1,
      "provenance"
    )
  ),
  # Why: Publish H1 hashes, profiles, and seeds as a model-run manifest.
  targets::tar_target(
    name = "table_human_event_h1_manifest",
    command = table_human_event_h1_provenance
  ),
  # Why: Assemble dataset and continent-age source tables from model outputs.
  targets::tar_target(
    name = "output_human_event_evidence_tables",
    command = build_human_event_evidence_tables(
      data_time_status = table_human_event_time_control_status,
      data_time_balance = table_human_event_time_balance_all_available,
      data_time_unique =
        table_human_event_time_control_unique_adjusted_r2,
      data_spatial_status = table_human_event_spatial_control_status,
      data_spatial_rankings = table_human_event_spatial_control_rankings,
      data_spatial_composition =
        table_human_event_spatial_control_composition,
      data_spatial_unique =
        table_human_event_spatial_control_unique_adjusted_r2,
      data_meta = data_human_event_meta
    )
  ),
  # Why: Publish every dataset result before matching or filtering.
  targets::tar_target(
    name = "table_human_event_dataset_all_available",
    command = output_human_event_evidence_tables[["dataset"]]
  ),
  # Why: Publish every continent-age result before matching or filtering.
  targets::tar_target(
    name = "table_human_event_region_age_all_available",
    command = output_human_event_evidence_tables[["region_age"]]
  ),
  # Why: Assert every reported facet carries only its permitted regional set.
  targets::tar_target(
    name = "table_human_event_result_predictor_audit",
    command = {
      expected <-
        table_human_event_predictor_manifest |>
        dplyr::select(
          dplyr::all_of(c(
            "region",
            "proxy_variant",
            "requested_regional_events",
            "retained_human_predictors",
            "reference_category"
          ))
        ) |>
        dplyr::distinct()
      observed <-
        dplyr::bind_rows(
          table_human_event_dataset_all_available,
          table_human_event_region_age_all_available
        ) |>
        dplyr::select(dplyr::all_of(names(expected))) |>
        dplyr::distinct()
      mismatch <-
        dplyr::bind_rows(
          dplyr::anti_join(observed, expected, by = names(expected)) |>
            dplyr::mutate(mismatch = "unexpected", .before = 1L),
          dplyr::anti_join(expected, observed, by = names(expected)) |>
            dplyr::mutate(mismatch = "missing", .before = 1L)
        )
      if (nrow(mismatch) > 0L) {
        cli::cli_abort("Result tables violate the regional predictor contract.")
      }
      observed
    }
  ),
  # Why: Match all three proxy variants only within cohort and dataset.
  targets::tar_target(
    name = "table_human_event_dataset_matched",
    command = prepare_human_event_matched_comparison(
      data_source = table_human_event_dataset_all_available,
      key_cols = c("dataset_id", "region", "climatezone"),
      metric_cols = c(
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
  # Why: Match all variants only within cohort, continent, and age.
  targets::tar_target(
    name = "table_human_event_region_age_matched",
    command = prepare_human_event_matched_comparison(
      data_source = table_human_event_region_age_all_available,
      key_cols = c("region", "age"),
      metric_cols = c(
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
  # Why: Summarise complete dataset distributions overall and geographically.
  targets::tar_target(
    name = "table_human_event_dataset_summary",
    command = dplyr::bind_rows(
      summarise_human_event_matched_comparison(
        table_human_event_dataset_matched,
        metric_cols = c(
          "human",
          "climate",
          "signed_difference",
          "signed_balance",
          "zero_balance",
          "unique_adjusted_r2_pure_human",
          "unique_adjusted_r2_pure_climate"
        )
      ),
      summarise_human_event_matched_comparison(
        table_human_event_dataset_matched,
        metric_cols = c(
          "human",
          "climate",
          "signed_difference",
          "signed_balance",
          "zero_balance",
          "unique_adjusted_r2_pure_human",
          "unique_adjusted_r2_pure_climate"
        ),
        group_cols = "region",
        summary_level = "continent"
      ),
      summarise_human_event_matched_comparison(
        table_human_event_dataset_matched,
        metric_cols = c(
          "human",
          "climate",
          "signed_difference",
          "signed_balance",
          "zero_balance",
          "unique_adjusted_r2_pure_human",
          "unique_adjusted_r2_pure_climate"
        ),
        group_cols = c("region", "climatezone"),
        summary_level = "climate_zone_within_continent"
      )
    )
  ),
  # Why: Summarise continent-age distributions without crossing continents.
  targets::tar_target(
    name = "table_human_event_region_age_summary",
    command = dplyr::bind_rows(
      summarise_human_event_matched_comparison(
        table_human_event_region_age_matched,
        metric_cols = c(
          "controlled_human",
          "controlled_climate",
          "controlled_balance",
          "zero_allocation_human",
          "zero_allocation_climate",
          "unique_adjusted_r2_pure_human",
          "unique_adjusted_r2_pure_climate"
        )
      ),
      summarise_human_event_matched_comparison(
        table_human_event_region_age_matched,
        metric_cols = c(
          "controlled_human",
          "controlled_climate",
          "controlled_balance",
          "zero_allocation_human",
          "zero_allocation_climate",
          "unique_adjusted_r2_pure_human",
          "unique_adjusted_r2_pure_climate"
        ),
        group_cols = "region",
        summary_level = "continent"
      )
    )
  ),
  # Why: Independently reconstruct every dataset summary from matched rows.
  targets::tar_target(
    name = "table_human_event_dataset_summary_reconciliation",
    command = diagnose_human_event_summary_reconciliation(
      data_comparison = table_human_event_dataset_matched,
      data_summary = table_human_event_dataset_summary
    )
  ),
  # Why: Independently reconstruct every continent-age summary from source.
  targets::tar_target(
    name = "table_human_event_region_age_summary_reconciliation",
    command = diagnose_human_event_summary_reconciliation(
      data_comparison = table_human_event_region_age_matched,
      data_summary = table_human_event_region_age_summary
    )
  ),
  # Why: Retain every ranking reversal and estimability change without a
  #   continuous-effect materiality threshold.
  targets::tar_target(
    name = "table_human_event_ranking_estimability_changes",
    command = dplyr::bind_rows(
      table_human_event_dataset_matched |>
        dplyr::filter(dplyr::if_any(
          dplyr::matches("reversal$|^estimability_change"),
          identity
        )) |>
        dplyr::mutate(analysis_scope = "dataset", .before = 1L),
      table_human_event_region_age_matched |>
        dplyr::filter(dplyr::if_any(
          dplyr::matches("reversal$|^estimability_change"),
          identity
        )) |>
        dplyr::mutate(analysis_scope = "continent_age", .before = 1L),
      table_human_event_spatiotemporal_matched |>
        dplyr::filter(dplyr::if_any(
          dplyr::matches("reversal$|^estimability_change"),
          identity
        )) |>
        dplyr::mutate(analysis_scope = "spatial_aggregation", .before = 1L)
    )
  ),
  # Why: Fit the event-only proxy below 2 ka, where SPD is unavailable, using
  #   all expert-screened records without rerunning the core sensitivity fits.
  targets::tar_target(
    name = "list_human_event_young_scenarios",
    command = list(
      list(cohort = "as_coded", seed_offset = 31000L)
    ),
    iteration = "list"
  ),
  targets::tar_target(
    name = "output_human_event_young_spatial",
    command = {
      cohort <- list_human_event_young_scenarios[["cohort"]]
      data_timebins <- prepare_human_event_young_timebins(
        data_timebins = data_human_event_full_timebins,
        data_chronologies = data_human_event_chronologies,
        cohort = cohort
      )
      predictor_resolver <- function(region, available_columns) {
        resolve_region_event_predictor_specification(
          region = region,
          proxy_variant = "events",
          available_columns = available_columns
        )
      }
      output_fit <- fit_spatial_hvarpart_dataset(
        data_source = data_timebins,
        analysis = "temporal_events",
        response_vars = human_event_response_variables,
        predictor_vars = predictor_resolver,
        permutations = human_event_analysis_config[["permutations"]],
        alpha = human_event_analysis_config[["alpha"]],
        min_unique_locations =
          human_event_analysis_config[["min_unique_locations"]],
        min_residual_df =
          human_event_analysis_config[["min_spatial_residual_df"]],
        distance_km =
          human_event_analysis_config[["spatial_distances_km"]],
        seed = human_event_analysis_config[["seed"]] +
          list_human_event_young_scenarios[["seed_offset"]]
      )
      summary_fit <- summarise_spatial_hvarpart_results(output_fit)

      composition <-
        prepare_spatial_hvarpart_composition(
          data_components = summary_fit[["components"]],
          data_status = summary_fit[["status"]]
        ) |>
        dplyr::filter(.data[["predictor"]] == "human") |>
        dplyr::transmute(
          .data[["analysis"]],
          .data[["region"]],
          .data[["age"]],
          zero_allocation_human = .data[["allocation"]]
        )

      rankings <- diagnose_spatial_hvarpart_rankings(
        data_components = summary_fit[["components"]],
        data_status = summary_fit[["status"]]
      ) |>
        dplyr::left_join(
          composition,
          by = c("analysis", "region", "age")
        ) |>
        dplyr::mutate(
          cohort = cohort,
          proxy_variant = "events",
          .before = 1L
        )
      list(cohort = cohort, rankings = rankings)
    },
    pattern = map(list_human_event_young_scenarios),
    iteration = "list"
  ),
  targets::tar_target(
    name = "table_human_event_young_spatial_rankings",
    command = output_human_event_young_spatial |>
      purrr::map(.f = ~ .x[["rankings"]]) |>
      dplyr::bind_rows()
  ),
  # Why: Combine the matched-period comparison with the younger event-only
  #   extension solely for reviewer-facing temporal display.
  targets::tar_target(
    name = "table_human_event_temporal_display",
    command = dplyr::bind_rows(
      table_human_event_region_age_all_available |>
        dplyr::select(dplyr::all_of(c(
          "cohort", "proxy_variant", "region", "age", "status",
          "zero_allocation_human"
        ))),
      table_human_event_young_spatial_rankings |>
        dplyr::select(dplyr::all_of(c(
          "cohort", "proxy_variant", "region", "age", "status",
          "zero_allocation_human"
        )))
    )
  ),
  # Why: Build the paired geographic figure in the finished SPD-radius style.
  targets::tar_target(
    name = "figure_human_event_spatial_comparison",
    command = plot_human_event_spatial_comparison(
      data_all_available = table_human_event_dataset_all_available,
      data_summary = table_human_event_dataset_summary
    )
  ),
  # Why: Build all-data temporal profiles and change figures, including the
  #   younger event-only interval, using the main zero-truncated composition.
  targets::tar_target(
    name = "output_human_event_temporal_figures",
    command = plot_human_event_temporal_split_comparison(
      data_all_available = table_human_event_temporal_display,
      data_event_extension = table_human_event_young_spatial_rankings,
      data_matched = table_human_event_region_age_matched
    )
  ),
  # Why: Export complete source, matched, summary, audit, and provenance tables.
  targets::tar_target(
    name = "files_human_event_evidence_tables",
    command = save_spd_radius_evidence_tables(
      data_tables = list(
        availability = table_human_event_availability,
        event_manifest = table_human_event_event_manifest,
        predictor_manifest = table_human_event_predictor_manifest,
        result_predictor_audit =
          table_human_event_result_predictor_audit,
        model_audit = table_human_event_model_audit,
        dataset_all_available = table_human_event_dataset_all_available,
        dataset_matched = table_human_event_dataset_matched,
        dataset_summary = table_human_event_dataset_summary,
        region_age_all_available =
          table_human_event_region_age_all_available,
        region_age_matched = table_human_event_region_age_matched,
        region_age_summary = table_human_event_region_age_summary,
        younger_event_only_spatial =
          table_human_event_young_spatial_rankings,
        dataset_summary_reconciliation =
          table_human_event_dataset_summary_reconciliation,
        region_age_summary_reconciliation =
          table_human_event_region_age_summary_reconciliation,
        ranking_estimability_changes =
          table_human_event_ranking_estimability_changes,
        spatiotemporal_estimates =
          table_human_event_spatiotemporal_estimates,
        spatiotemporal_matched =
          table_human_event_spatiotemporal_matched,
        spatiotemporal_summary =
          table_human_event_spatiotemporal_summary,
        h1_provenance = table_human_event_h1_provenance,
        h1_manifest = table_human_event_h1_manifest
      ),
      file_paths = c(
        availability = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__chronology_availability.csv"
        ),
        event_manifest = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__event_manifest.csv"
        ),
        predictor_manifest = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__predictor_manifest.csv"
        ),
        result_predictor_audit = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__result_predictor_audit.csv"
        ),
        model_audit = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__regional_model_audit.csv"
        ),
        dataset_all_available = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__dataset_all_available.csv"
        ),
        dataset_matched = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__dataset_matched.csv"
        ),
        dataset_summary = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__dataset_summary.csv"
        ),
        region_age_all_available = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__region_age_all_available.csv"
        ),
        region_age_matched = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__region_age_matched.csv"
        ),
        region_age_summary = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__region_age_summary.csv"
        ),
        younger_event_only_spatial = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          paste0(
            "events__inclusion_comparison__",
            "younger_event_only_spatial.csv"
          )
        ),        dataset_summary_reconciliation = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          paste0(
            "events__inclusion_comparison__",
            "dataset_summary_reconciliation.csv"
          )
        ),
        region_age_summary_reconciliation = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          paste0(
            "events__inclusion_comparison__",
            "region_age_summary_reconciliation.csv"
          )
        ),
        ranking_estimability_changes = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          paste0(
            "events__inclusion_comparison__",
            "ranking_estimability_changes.csv"
          )
        ),
        spatiotemporal_estimates = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__spatiotemporal_estimates.csv"
        ),
        spatiotemporal_matched = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__spatiotemporal_matched.csv"
        ),
        spatiotemporal_summary = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__spatiotemporal_summary.csv"
        ),
        h1_provenance = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__h1_provenance.csv"
        ),
        h1_manifest = here::here(
          "Outputs/Tables/H1/Sensitivity/Human_event_inclusion",
          "events__inclusion_comparison__h1_manifest.csv"
        )
      )
    ),
    format = "file"
  ),
  # Why: Save the spatial figure and four full-width temporal figures with
  #   semantic, identically stemmed PNG/PDF filenames.
  targets::tar_target(
    name = "files_human_event_sensitivity_figures",
    command = save_human_event_sensitivity_figures(
      plot_spatial = figure_human_event_spatial_comparison,
      temporal_plots = output_human_event_temporal_figures,
      path_spatial = here::here(
        "Outputs/Figures/H1/Sensitivity/Human_event_inclusion",
        paste0(
          "events__inclusion_comparison__spatial_",
          "zero_truncated_human_climate_balance"
        )
      ),
      temporal_paths = c(
        profiles = here::here(
          "Outputs/Figures/H1/Sensitivity/Human_event_inclusion",
          paste0(
            "events__inclusion_comparison__",
            "temporal_zero_truncated_human_share__",
            "space_control__all_data"
          )
        ),
        changes = here::here(
          "Outputs/Figures/H1/Sensitivity/Human_event_inclusion",
          paste0(
            "events__inclusion_comparison__",
            "temporal_zero_truncated_human_share_change__",
            "space_control__all_data"
          )
        )
      )
    ),
    format = "file"
  ),
  # Why: Hash every exported table and figure for response traceability.
  targets::tar_target(
    name = "table_human_event_evidence_manifest",
    command = {
      table_paths <- unname(files_human_event_evidence_tables)
      figure_paths <- unname(files_human_event_sensitivity_figures)
      evidence_index <- tibble::tibble(
        artifact_id = c(
          paste0(
            "table_",
            tools::file_path_sans_ext(basename(table_paths))
          ),
          paste0("figure_", seq_along(figure_paths))
        ),
        description = c(
          paste("Source table", basename(table_paths)),
          paste("Sensitivity figure", basename(figure_paths))
        ),
        analysis_profile = "human_event_inclusion",
        source_pipeline = paste0(
          "91_sensitivity_analyses/",
          "human_event_inclusion/pipeline.R"
        ),
        public_target = c(
          rep("files_human_event_evidence_tables", length(table_paths)),
          rep("files_human_event_sensitivity_figures", length(figure_paths))
        ),
        path = c(table_paths, figure_paths)
      )
      build_evidence_manifest(evidence_index)
    }
  ),
  # Why: Publish the sensitivity-specific evidence manifest as stable CSV.
  targets::tar_target(
    name = "file_human_event_evidence_manifest",
    command = {
      path_output <- here::here(
        "Outputs/Tables/Reporting",
        "events__inclusion_comparison__evidence_manifest.csv"
      )
      dir.create(dirname(path_output), recursive = TRUE, showWarnings = FALSE)
      readr::write_csv(table_human_event_evidence_manifest, path_output)
      path_output
    },
    format = "file"
  )
)
