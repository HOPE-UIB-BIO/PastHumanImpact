#----------------------------------------------------------#
#
#                     GlobalHumanImpact
#
#     Human-proxy H1 HVarPart design diagnostics: climate
#     reduction, residual-df record loss, and age detrending
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Run with:
#   R/analyses/91_sensitivity_analyses/human_proxy_hvarpart_design_diagnostics/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Methods diagnostics on the matched 2-8 ka cohort. Canonical H1 and H2
# stores and outputs are read only and never modified.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

runner_h1 <- "R/analyses/02_h1_spatiotemporal_hvarpart/00_run.R"

runner_proxy <-
  "R/analyses/91_sensitivity_analyses/human_proxy_convergence/00_run.R"

store_h1 <-
  resolve_pipeline_store_path(data_storage_path, "analyses_h1/inputs")

store_proxy <-
  resolve_pipeline_store_path(
    data_storage_path,
    "sensitivity_analyses/human_proxy_convergence"
  )

path_profiles <-
  here::here("R", "analyses", "00_profiles", "analysis_profiles.csv")

path_tables <-
  here::here(
    "Outputs", "Tables", "H1", "Sensitivity",
    "Human_proxy_hvarpart_design_diagnostics"
  )

config <-
  list(
    configuration_reference = "human_proxy_hvarpart_design_diagnostics",
    age_min = 2000,
    age_max = 8000,
    min_unique_ages = 10L,
    fit_min_temporal_residual_df = 4L,
    reported_residual_df = c(4L, 5L),
    max_cor = 0.8,
    max_vif = 5,
    climate = c("temp_annual", "temp_cold", "prec_summer", "prec_win"),
    proxy_columns = c("spd_sqrt", "kk10_fraction", "hyde_sqrt")
  )

models_climate_reduction <-
  c(reference = "spd__full_climate", comparison = "spd__filtered_climate")

models_age_detrending <-
  c(
    "spd__filtered_climate", "spd_age_detrended__filtered_climate",
    "kk10__filtered_climate", "kk10_age_detrended__filtered_climate",
    "hyde__filtered_climate", "hyde_age_detrended__filtered_climate"
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track the canonical H1 inputs without rebuilding their store.
  targets::tar_target(
    name = "fingerprint_designdiag_h1",
    command = compute_target_store_fingerprint(
      store_h1,
      c(
        "data_hvar_filtered_unique_age", "data_hvar_timebins_spd_unique_age",
        "data_meta", "h1_response_variables", "h1_analysis_config"
      ),
      runner_h1
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track the matched SPD, KK10, and HYDE observations.
  targets::tar_target(
    name = "fingerprint_designdiag_proxy",
    command = compute_target_store_fingerprint(
      store_proxy, "data_human_proxy_matches", runner_proxy
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Track registry changes as part of the analysis definition.
  targets::tar_target(
    name = "file_designdiag_profiles",
    command = path_profiles,
    format = "file"
  ),
  # Why: Import canonical within-dataset response and predictor histories.
  targets::tar_target(
    name = "data_designdiag_within_source",
    command = {
      force(fingerprint_designdiag_h1)
      load_target_store_value(
        store_h1, "data_hvar_filtered_unique_age", runner_h1
      )
    }
  ),
  # Why: Import canonical region-by-age response and predictor tables.
  targets::tar_target(
    name = "data_designdiag_slice_source",
    command = {
      force(fingerprint_designdiag_h1)
      load_target_store_value(
        store_h1, "data_hvar_timebins_spd_unique_age", runner_h1
      )
    }
  ),
  # Why: Import dataset metadata for coordinates, regions, and climate zones.
  targets::tar_target(
    name = "data_designdiag_metadata",
    command = {
      force(fingerprint_designdiag_h1)
      load_target_store_value(store_h1, "data_meta", runner_h1)
    }
  ),
  # Why: Import the canonical multivariate response columns.
  targets::tar_target(
    name = "designdiag_response_variables",
    command = {
      force(fingerprint_designdiag_h1)
      load_target_store_value(store_h1, "h1_response_variables", runner_h1)
    }
  ),
  # Why: Import canonical seed, permutations, distances, and spatial minimums.
  targets::tar_target(
    name = "designdiag_h1_config",
    command = {
      force(fingerprint_designdiag_h1)
      load_target_store_value(store_h1, "h1_analysis_config", runner_h1)
    }
  ),
  # Why: Import the matched proxy observations shared by all three proxies.
  targets::tar_target(
    name = "data_designdiag_proxy_matches",
    command = {
      force(fingerprint_designdiag_proxy)
      load_target_store_value(
        store_proxy, "data_human_proxy_matches", runner_proxy
      )
    }
  ),
  # Why: Define the seven single-proxy model variants once.
  targets::tar_target(
    name = "data_designdiag_specs",
    command = build_human_proxy_diagnostic_specifications()
  ),
  # Why: Fail early if the registered profiles and the model variants differ.
  targets::tar_target(
    name = "data_designdiag_profiles",
    command = {
      data_profiles <-
        load_analysis_profiles(file_designdiag_profiles) |>
        dplyr::filter(
          .data[["configuration_reference"]] ==
            config[["configuration_reference"]]
        )

      vec_spec_profiles <-
        c(
          data_designdiag_specs[["within_dataset_profile_id"]],
          stats::na.omit(data_designdiag_specs[["time_slice_profile_id"]])
        )

      assertthat::assert_that(
        setequal(data_profiles[["profile_id"]], vec_spec_profiles),
        all(data_profiles[["enabled"]]),
        msg = "Design-diagnostic profiles and model variants disagree."
      )

      data_profiles
    }
  ),
  # Why: Join matched proxies to canonical inputs on one 2-8 ka cohort.
  targets::tar_target(
    name = "output_designdiag_inputs",
    command = prepare_collinearity_managed_human_proxy_inputs(
      data_designdiag_within_source, data_designdiag_slice_source,
      data_designdiag_proxy_matches, data_designdiag_metadata,
      config[["age_min"]], config[["age_max"]]
    )
  ),
  # Why: Add each proxy detrended on linear age within each record.
  targets::tar_target(
    name = "data_designdiag_within",
    command = prepare_age_detrended_predictors(
      data_source = output_designdiag_inputs[["within_dataset"]],
      predictor_columns = config[["proxy_columns"]]
    )
  ),
  # Why: Prove detrended proxies are uncorrelated with age in every record.
  targets::tar_target(
    name = "table_designdiag_detrending_check",
    command = validate_age_detrended_predictors(
      data_source = data_designdiag_within,
      detrended_columns = stringr::str_c(
        config[["proxy_columns"]], "_age_detrended"
      )
    )
  ),
  # Why: Select climate once per record and give the full-climate arm all
  # four variables on identical data.
  targets::tar_target(
    name = "data_designdiag_temporal_designs",
    command = prepare_local_hvarpart_model_designs(
      data_source = data_designdiag_within,
      model_specifications = data_designdiag_specs,
      unit_columns = "dataset_id",
      climate_candidates = config[["climate"]],
      max_cor = config[["max_cor"]],
      max_vif = config[["max_vif"]]
    ) |>
      prepare_full_climate_hvarpart_designs(
        model_ids = models_climate_reduction[["reference"]],
        climate_candidates = config[["climate"]]
      )
  ),
  # Why: Build the region-age designs for the two climate-reduction arms.
  targets::tar_target(
    name = "data_designdiag_spatial_designs",
    command = prepare_local_hvarpart_model_designs(
      data_source = output_designdiag_inputs[["time_slices"]],
      model_specifications = data_designdiag_specs |>
        dplyr::filter(!is.na(.data[["time_slice_profile_id"]])),
      unit_columns = c("region", "age"),
      climate_candidates = config[["climate"]],
      max_cor = config[["max_cor"]],
      max_vif = config[["max_vif"]]
    ) |>
      prepare_full_climate_hvarpart_designs(
        model_ids = models_climate_reduction[["reference"]],
        climate_candidates = config[["climate"]]
      )
  ),
  # Why: Fit every record once at the lowest reported minimum; higher
  # minimums are read from the stored residual degrees of freedom.
  targets::tar_target(
    name = "output_designdiag_temporal_fits",
    command = fit_local_temporal_hvarpart_models(
      data_designs = data_designdiag_temporal_designs,
      response_vars = designdiag_response_variables,
      seed = designdiag_h1_config[["seed"]],
      min_unique_ages = config[["min_unique_ages"]],
      min_residual_df = config[["fit_min_temporal_residual_df"]],
      distance_years = designdiag_h1_config[["temporal_distances_years"]],
      permutations = designdiag_h1_config[["permutations"]]
    )
  ),
  # Why: Fit the region-age climate-reduction arms with canonical controls.
  targets::tar_target(
    name = "output_designdiag_spatial_fits",
    command = fit_local_spatial_hvarpart_models(
      data_designs = data_designdiag_spatial_designs,
      response_vars = designdiag_response_variables,
      seed = designdiag_h1_config[["seed"]],
      permutations = designdiag_h1_config[["permutations"]],
      alpha = designdiag_h1_config[["alpha"]],
      min_unique_locations = designdiag_h1_config[["min_unique_locations"]],
      min_residual_df = designdiag_h1_config[["min_spatial_residual_df"]],
      distance_km = designdiag_h1_config[["spatial_distances_km"]]
    )
  ),
  # Why: Halt on any caught fit error instead of reporting partial results.
  targets::tar_target(
    name = "check_designdiag_model_errors",
    command = {
      data_errors <-
        dplyr::bind_rows(
          output_designdiag_temporal_fits |>
            dplyr::filter(!is.na(.data[["error_message"]])) |>
            dplyr::transmute(
              path = "time_control",
              unit = .data[["dataset_id"]],
              model_id = .data[["model_id"]],
              error_message = .data[["error_message"]]
            ),
          output_designdiag_spatial_fits |>
            dplyr::filter(!is.na(.data[["error_message"]])) |>
            dplyr::transmute(
              path = "spatial_control",
              unit = stringr::str_c(.data[["region"]], .data[["age"]],
                sep = " "),
              model_id = .data[["model_id"]],
              error_message = .data[["error_message"]]
            )
        )

      if (
        nrow(data_errors) > 0L
      ) {
        cli::cli_abort(c(
          "HVarPart model errors occurred.",
          "x" = paste(
            utils::capture.output(print(data_errors, n = Inf)),
            collapse = "\n"
          )
        ))
      }

      TRUE
    }
  ),
  # Why: Extract status and component tables for every per-record model.
  targets::tar_target(
    name = "result_designdiag_temporal",
    command = {
      force(check_designdiag_model_errors)
      summarise_local_temporal_hvarpart_models(
        output_designdiag_temporal_fits,
        analysis_prefix = "spatial_human_proxy_design_diagnostic"
      )
    }
  ),
  # Why: Extract status and component tables for every region-age model.
  targets::tar_target(
    name = "result_designdiag_spatial",
    command = {
      force(check_designdiag_model_errors)
      summarise_local_spatial_hvarpart_models(output_designdiag_spatial_fits)
    }
  ),
  # Why: Expose the stored commonality fractions without changing any fit.
  targets::tar_target(
    name = "data_designdiag_commonality",
    command = {
      force(check_designdiag_model_errors)
      summarise_hvarpart_commonality_fractions(
        data_fits = output_designdiag_temporal_fits,
        id_columns = c("model_id", "dataset_id")
      )
    }
  ),
  # Why: Prove the commonality fractions sum to each model's adjusted R2.
  targets::tar_target(
    name = "table_designdiag_commonality_check",
    command = validate_hvarpart_commonality_totals(
      data_commonality = data_designdiag_commonality,
      data_components = result_designdiag_temporal[["components"]]
    )
  ),
  # Why: Convert components to per-record human-climate balances per model.
  targets::tar_target(
    name = "data_designdiag_balance_records",
    command = data_designdiag_specs[["model_id"]] |>
      purrr::map(
        .f = ~ {
          model_id_current <- .x

          prepare_time_controlled_importance_records(
            data_components = result_designdiag_temporal[["components"]] |>
              dplyr::filter(.data[["model_id"]] == model_id_current),
            data_status = result_designdiag_temporal[["status"]] |>
              dplyr::filter(.data[["model_id"]] == model_id_current),
            data_meta = data_designdiag_metadata
          ) |>
            dplyr::mutate(model_id = model_id_current, .before = 1L)
        }
      ) |>
      dplyr::bind_rows()
  ),
  # Why: Attach continental regions to statuses for the record-loss counts.
  targets::tar_target(
    name = "data_designdiag_status_regions",
    command = result_designdiag_temporal[["status"]] |>
      dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
      dplyr::left_join(
        data_designdiag_metadata |>
          dplyr::transmute(
            dataset_id = as.character(.data[["dataset_id"]]),
            region = .data[["region"]]
          ),
        by = "dataset_id"
      )
  ),
  # Why: Count records lost under each residual-df minimum (D12).
  targets::tar_target(
    name = "table_designdiag_record_loss",
    command = summarise_residual_df_record_loss(
      data_status = data_designdiag_status_regions,
      thresholds = config[["reported_residual_df"]],
      fit_min_residual_df = config[["fit_min_temporal_residual_df"]]
    )
  ),
  # Why: Show where the record loss falls geographically.
  targets::tar_target(
    name = "table_designdiag_record_loss_by_region",
    command = summarise_residual_df_record_loss(
      data_status = data_designdiag_status_regions,
      thresholds = config[["reported_residual_df"]],
      fit_min_residual_df = config[["fit_min_temporal_residual_df"]],
      group_columns = "region"
    )
  ),
  # Why: List every matched comparison of the two tests and minimums.
  targets::tar_target(
    name = "data_designdiag_comparison_plan",
    command = dplyr::bind_rows(
      tidyr::expand_grid(
        test = "climate_reduction",
        proxy = "spd",
        cohort = "pair",
        min_residual_df = config[["reported_residual_df"]]
      ) |>
        dplyr::mutate(
          reference_model_id = models_climate_reduction[["reference"]],
          comparison_model_id = models_climate_reduction[["comparison"]]
        ),
      tidyr::expand_grid(
        test = "age_detrending",
        proxy = c("spd", "kk10", "hyde"),
        cohort = c("common", "proxy"),
        min_residual_df = config[["reported_residual_df"]]
      ) |>
        dplyr::mutate(
          reference_model_id = stringr::str_c(
            .data[["proxy"]], "__filtered_climate"
          ),
          comparison_model_id = stringr::str_c(
            .data[["proxy"]], "_age_detrended__filtered_climate"
          )
        )
    ) |>
      dplyr::mutate(
        cohort_model_ids = purrr::pmap(
          list(
            .data[["cohort"]],
            .data[["reference_model_id"]],
            .data[["comparison_model_id"]]
          ),
          .f = ~ if (identical(..1, "common")) {
            models_age_detrending
          } else {
            c(..2, ..3)
          }
        )
      )
  ),
  # Why: Match records and summarise each comparison on identical records.
  targets::tar_target(
    name = "output_designdiag_comparisons",
    command = seq_len(nrow(data_designdiag_comparison_plan)) |>
      purrr::map(
        .f = ~ {
          data_plan_row <- data_designdiag_comparison_plan[.x, ]

          summarise_matched_hvarpart_model_comparison(
            data_records = data_designdiag_balance_records,
            data_status = result_designdiag_temporal[["status"]],
            data_commonality = data_designdiag_commonality,
            cohort_model_ids = data_plan_row[["cohort_model_ids"]][[1]],
            reference_model_id = data_plan_row[["reference_model_id"]],
            comparison_model_id = data_plan_row[["comparison_model_id"]],
            min_residual_df = data_plan_row[["min_residual_df"]],
            comparison_labels = data_plan_row |>
              dplyr::select(dplyr::all_of(c("test", "proxy", "cohort")))
          )
        }
      )
  ),
  # Why: Spatially aggregate the balances of each distinct matched cohort
  # with canonical dbMEM control (overall, regional, and stratum estimates).
  targets::tar_target(
    name = "output_designdiag_spatial_balance",
    command = {
      vec_aggregate_rows <-
        which(
          data_designdiag_comparison_plan[["test"]] == "climate_reduction" |
            (data_designdiag_comparison_plan[["cohort"]] == "common" &
              data_designdiag_comparison_plan[["proxy"]] == "spd")
        )

      vec_aggregate_rows |>
        purrr::map(
          .f = ~ {
            data_plan_row <- data_designdiag_comparison_plan[.x, ]

            data_records <-
              output_designdiag_comparisons[[.x]][["records"]]

            fit_human_proxy_spatial_balance_models(
              data_records = data_records,
              seed = designdiag_h1_config[["seed"]],
              permutations = designdiag_h1_config[["permutations"]],
              alpha = designdiag_h1_config[["alpha"]],
              min_unique_locations =
                designdiag_h1_config[["min_unique_locations"]],
              min_residual_df =
                designdiag_h1_config[["min_spatial_residual_df"]],
              distance_km = designdiag_h1_config[["spatial_distances_km"]]
            ) |>
              dplyr::mutate(
                test = data_plan_row[["test"]],
                cohort = data_plan_row[["cohort"]],
                min_residual_df = data_plan_row[["min_residual_df"]],
                .before = 1L
              )
          }
        ) |>
        dplyr::bind_rows()
    }
  ),
  # Why: Flatten the dbMEM-adjusted estimates of every aggregation.
  targets::tar_target(
    name = "table_designdiag_spatial_balance_estimates",
    command = output_designdiag_spatial_balance |>
      dplyr::mutate(
        estimates = purrr::map(.data[["result"]], "estimates")
      ) |>
      dplyr::select(
        dplyr::all_of(c(
          "test", "cohort", "min_residual_df", "model_id", "estimates"
        ))
      ) |>
      tidyr::unnest(cols = dplyr::all_of("estimates"))
  ),
  # Why: Prepare region-age compositions for the two climate arms.
  targets::tar_target(
    name = "output_designdiag_spatial_comparisons",
    command = models_climate_reduction |>
      unname() |>
      purrr::map(
        .f = ~ prepare_human_proxy_comparison_tables(
          temporal_summary = result_designdiag_temporal,
          spatial_summary = result_designdiag_spatial,
          data_metadata = data_designdiag_metadata,
          model_id = .x
        )
      ) |>
      rlang::set_names(unname(models_climate_reduction))
  ),
  # Why: Compare the climate arms on identical region-age units.
  targets::tar_target(
    name = "output_designdiag_composition_change",
    command = summarise_paired_spatial_control_composition_change(
      data_composition = output_designdiag_spatial_comparisons |>
        purrr::map("composition_common") |>
        dplyr::bind_rows(),
      reference_model_id = models_climate_reduction[["reference"]],
      comparison_model_id = models_climate_reduction[["comparison"]]
    )
  ),
  # Why: Record the configuration and data versions behind every table.
  targets::tar_target(
    name = "table_designdiag_provenance",
    command = tibble::tribble(
      ~field, ~value,
      "age_window_bp", stringr::str_c(config[["age_min"]], "-",
        config[["age_max"]]),
      "min_unique_ages", as.character(config[["min_unique_ages"]]),
      "fit_min_temporal_residual_df",
      as.character(config[["fit_min_temporal_residual_df"]]),
      "reported_temporal_residual_df",
      stringr::str_c(config[["reported_residual_df"]], collapse = ";"),
      "min_spatial_residual_df",
      as.character(designdiag_h1_config[["min_spatial_residual_df"]]),
      "climate_candidates", stringr::str_c(config[["climate"]],
        collapse = ";"),
      "climate_filter", stringr::str_c(
        "max_cor=", config[["max_cor"]], ";max_vif=", config[["max_vif"]]
      ),
      "spd", "spd_sqrt: square root of matched SPD",
      "kk10", "kk10_fraction: KK10 land_use fraction, identity",
      "hyde", "hyde_sqrt: square root of HYDE 3.2 popc population",
      "age_detrending", "residuals of proxy ~ 1 + age within each record",
      "permutations", as.character(designdiag_h1_config[["permutations"]]),
      "seed", as.character(designdiag_h1_config[["seed"]]),
      "fingerprint_h1_inputs", as.character(fingerprint_designdiag_h1),
      "fingerprint_proxy_matches", as.character(fingerprint_designdiag_proxy)
    )
  ),
  # Why: Export every diagnostic table under stable semantic file names.
  targets::tar_target(
    name = "files_designdiag_tables",
    command = {
      data_comparisons <-
        c(
          "cohort", "exclusions", "pairs", "quantity_summary",
          "ranking_summary", "commonality_values", "commonality_summary"
        ) |>
        rlang::set_names() |>
        purrr::map(
          .f = ~ output_designdiag_comparisons |>
            purrr::map(.x) |>
            dplyr::bind_rows()
        )

      list_by_test <-
        purrr::map(
          c(
            climate_reduction = "climate_reduction",
            age_detrending = "age_detrending"
          ),
          .f = ~ {
            test_name <- .x

            purrr::map(
              data_comparisons,
              .f = ~ dplyr::filter(.x, .data[["test"]] == test_name)
            )
          }
        )

      data_estimates <-
        table_designdiag_spatial_balance_estimates

      list_tables <-
        list(
          spd__climate_reduction__cohorts__time_control =
            list_by_test[["climate_reduction"]][["cohort"]],
          spd__climate_reduction__pairing_exclusions__time_control =
            list_by_test[["climate_reduction"]][["exclusions"]],
          spd__climate_reduction__dataset_pairs__time_control =
            list_by_test[["climate_reduction"]][["pairs"]],
          spd__climate_reduction__balance_summary__time_control =
            list_by_test[["climate_reduction"]][["quantity_summary"]],
          spd__climate_reduction__ranking_summary__time_control =
            list_by_test[["climate_reduction"]][["ranking_summary"]],
          spd__climate_reduction__unit_pairs__space_control =
            output_designdiag_composition_change[["unit_pairs"]],
          spd__climate_reduction__composition_summary__space_control =
            output_designdiag_composition_change[["summary"]],
          spd__climate_reduction__unit_exclusions__space_control =
            output_designdiag_composition_change[["exclusions"]],
          spd__model_status__space_control =
            result_designdiag_spatial[["status"]],
          human_proxies__model_status__time_control =
            data_designdiag_status_regions,
          human_proxies__residual_df_record_loss__time_control =
            table_designdiag_record_loss,
          human_proxies__residual_df_record_loss_by_region__time_control =
            table_designdiag_record_loss_by_region,
          human_proxies__age_detrending__cohorts__time_control =
            list_by_test[["age_detrending"]][["cohort"]],
          human_proxies__age_detrending__pairing_exclusions__time_control =
            list_by_test[["age_detrending"]][["exclusions"]],
          human_proxies__age_detrending__dataset_pairs__time_control =
            list_by_test[["age_detrending"]][["pairs"]],
          human_proxies__age_detrending__balance_summary__time_control =
            list_by_test[["age_detrending"]][["quantity_summary"]],
          human_proxies__age_detrending__ranking_summary__time_control =
            list_by_test[["age_detrending"]][["ranking_summary"]],
          human_proxies__human_time_commonality__dataset_values__time_control =
            data_comparisons[["commonality_values"]],
          human_proxies__human_time_commonality__summary__time_control =
            data_comparisons[["commonality_summary"]],
          human_proxies__age_detrending__orthogonality_check =
            table_designdiag_detrending_check,
          human_proxies__commonality_total_check__time_control =
            table_designdiag_commonality_check,
          human_proxies__design_diagnostics__provenance =
            table_designdiag_provenance
        )

      vec_estimate_stems <-
        c(
          climate_reduction = stringr::str_c(
            "spd__climate_reduction__spatial_balance_estimates__",
            "time_and_space_control"
          ),
          age_detrending = stringr::str_c(
            "human_proxies__age_detrending__spatial_balance_estimates__",
            "time_and_space_control"
          )
        )

      list_estimates <-
        names(vec_estimate_stems) |>
        purrr::map(
          .f = ~ dplyr::filter(data_estimates, .data[["test"]] == .x)
        ) |>
        rlang::set_names(unname(vec_estimate_stems))

      list_tables_all <-
        c(list_tables, list_estimates)

      vec_paths <-
        file.path(
          path_tables,
          stringr::str_c(names(list_tables_all), ".csv")
        ) |>
        rlang::set_names(names(list_tables_all))

      save_collinearity_managed_hvarpart_tables(list_tables_all, vec_paths)
    },
    format = "file"
  )
)
