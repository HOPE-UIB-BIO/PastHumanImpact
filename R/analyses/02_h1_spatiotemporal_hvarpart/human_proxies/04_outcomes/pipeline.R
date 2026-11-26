#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#        Single-proxy H1 outcomes: cross-run checks, decision
#         metrics on shared units, outcome table, and the
#                reproduction of the reference SPD run
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#
# Defines the outcome graph of the single-proxy H1 runs.
# Run with:
#   R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R
# Sourcing this script only declares targets; it does not execute them.
# Reads every single-proxy run store (analyses_h1/<profile_id>) and the
#   reference collinearity-managed store and tables; all are read only.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

store_inputs <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h1/human_proxy_inputs"
  )

runner_h1_proxies <-
  "R/analyses/02_h1_spatiotemporal_hvarpart/human_proxies/00_run.R"

path_tables <-
  here::here("Outputs", "Tables", "H1", "Proxies", "Outcomes")

path_reference_tables <-
  here::here(
    "Outputs", "Tables", "H1", "Sensitivity",
    "Human_proxy_hvarpart_collinearity_managed"
  )

# The reference SPD model (matched SPD bridge of the collinearity-managed
#   sensitivity) is refitted from its own inputs to check the code; its store
#   is read only.
store_reference <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path =
      "sensitivity_analyses/human_proxy_hvarpart_collinearity_managed"
  )

runner_reference <-
  stringr::str_c(
    "R/analyses/91_sensitivity_analyses/",
    "human_proxy_hvarpart_collinearity_managed/00_run.R"
  )

vec_run_public_targets <-
  c(
    "table_h1_proxy_status",
    "data_h1_proxy_run_units",
    "table_h1_proxy_climate_used",
    "data_h1_proxy_estimable_units",
    "table_h1_proxy_decision_metric"
  )

config_h1_proxy_outcomes <-
  list(
    main_proxies = c("spd", "kk10", "hyde_3_5_land_use"),
    supplementary_proxies = c("hyde_3_5_population", "events"),
    # Places each run's own metric (all records) in the outcome table: SPD,
    #   always at its buffer, belongs to both support columns of the main
    #   proxies; supplementary proxies are listed at their own support.
    support_map = tibble::tribble(
      ~comparison, ~human_proxy, ~proxy_support, ~gridded_support,
      "main_proxies", "spd", "spd_buffer", "spd_buffer",
      "main_proxies", "spd", "spd_buffer", "radius_50km",
      "main_proxies", "kk10", "spd_buffer", "spd_buffer",
      "main_proxies", "kk10", "radius_50km", "radius_50km",
      "main_proxies", "hyde_3_5_land_use", "spd_buffer", "spd_buffer",
      "main_proxies", "hyde_3_5_land_use", "radius_50km", "radius_50km",
      "supplementary", "hyde_3_5_population", "spd_buffer", "spd_buffer",
      "supplementary", "events", "not_applicable", "not_applicable"
    ),
    reference_model_id = "spd_matched_bridge",
    reference_human_column = "spd_sqrt",
    # The reference run fitted records with at least four residual df.
    reference_min_residual_df = 4L,
    reproduction_profile_id =
      "within_dataset_spd_spd_buffer_common_cohort_time_control",
    reproduction_tolerance = 1e-8
  )

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Fingerprint the shared inputs used by the cross-run checks.
  targets::tar_target(
    name = "fingerprint_h1_proxy_inputs",
    command = compute_target_store_fingerprint(
      store = store_inputs,
      target_names = c(
        "data_h1_proxy_specifications",
        "h1_proxy_config",
        "h1_proxy_response_variables",
        "data_h1_proxy_meta",
        "data_h1_proxy_common_cohort",
        "data_h1_proxy_record_climate",
        "data_h1_proxy_slice_climate"
      ),
      runner = runner_h1_proxies
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Import the shared response columns for the reference refit.
  targets::tar_target(
    name = "h1_proxy_response_variables",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "h1_proxy_response_variables",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: Import record regions, climate zones, and coordinates for the
  #   reference refit balances.
  targets::tar_target(
    name = "data_h1_proxy_meta",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_meta",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: Import every run specification, which names the run stores.
  targets::tar_target(
    name = "data_h1_proxy_specifications",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_specifications",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: Import the shared seed, thresholds, and bootstrap settings.
  targets::tar_target(
    name = "h1_proxy_config",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "h1_proxy_config",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: Import the common cohort for the reproduction check.
  targets::tar_target(
    name = "data_h1_proxy_common_cohort",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_common_cohort",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: Import the climate block selected once per record and window.
  targets::tar_target(
    name = "data_h1_proxy_record_climate",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_record_climate",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: Import the climate block selected once per region-by-age unit.
  targets::tar_target(
    name = "data_h1_proxy_slice_climate",
    command = {
      fingerprint_h1_proxy_inputs

      load_target_store_value(
        store = store_inputs,
        target_name = "data_h1_proxy_slice_climate",
        runner = runner_h1_proxies
      )
    }
  ),
  # Why: List every run with its scale, proxy, support, window, and store.
  targets::tar_target(
    name = "data_h1_proxy_runs",
    command = data_h1_proxy_specifications |>
      dplyr::transmute(
        profile_id = .data[["model_id"]],
        analysis = dplyr::if_else(
          .data[["analytical_unit"]] == "within_dataset",
          "spatial_h1",
          "temporal_h1"
        ),
        human_proxy = .data[["human_proxy"]],
        proxy_support = .data[["proxy_support"]],
        age_window = .data[["age_window"]],
        store = purrr::map_chr(
          .data[["model_id"]],
          ~ resolve_pipeline_store_path(
            data_storage_path = data_storage_path,
            store_relative_path = file.path("analyses_h1", .x)
          )
        )
      )
  ),
  # Why: Fingerprint every run store so a refitted run invalidates the
  #   outcomes.
  targets::tar_target(
    name = "fingerprint_h1_proxy_runs",
    command = data_h1_proxy_runs |>
      dplyr::pull("store") |>
      purrr::map_chr(
        .f = ~ compute_target_store_fingerprint(
          store = .x,
          target_names = vec_run_public_targets,
          runner = runner_h1_proxies
        )
      ) |>
      rlang::set_names(dplyr::pull(data_h1_proxy_runs, "profile_id")),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Import the public results of every run once.
  targets::tar_target(
    name = "list_h1_proxy_run_results",
    command = {
      fingerprint_h1_proxy_runs

      data_h1_proxy_runs |>
        dplyr::pull("store") |>
        purrr::map(
          .f = ~ {
            store_run <- .x

            vec_run_public_targets |>
              rlang::set_names() |>
              purrr::map(
                .f = ~ load_target_store_value(
                  store = store_run,
                  target_name = .x,
                  runner = runner_h1_proxies
                )
              )
          }
        ) |>
        rlang::set_names(dplyr::pull(data_h1_proxy_runs, "profile_id"))
    }
  ),
  # Why: Bind one public result table across the runs of one scale.
  targets::tar_target(
    name = "list_h1_proxy_results_by_scale",
    command = c("spatial_h1", "temporal_h1") |>
      rlang::set_names() |>
      purrr::map(
        .f = ~ {
          vec_profiles <-
            data_h1_proxy_runs |>
            dplyr::filter(.data[["analysis"]] == .x) |>
            dplyr::pull("profile_id")

          vec_run_public_targets |>
            rlang::set_names() |>
            purrr::map(
              .f = ~ {
                target_name <- .x

                list_h1_proxy_run_results[vec_profiles] |>
                  purrr::map(target_name) |>
                  dplyr::bind_rows()
              }
            )
        }
      )
  ),
  # Why: Prove that every spatial H1 run used the climate block selected
  #   once for each record and window.
  targets::tar_target(
    name = "table_h1_proxy_shared_climate_check_records",
    command = validate_shared_climate_selection_across_runs(
      data_used =
        list_h1_proxy_results_by_scale[["spatial_h1"]][[
          "table_h1_proxy_climate_used"
        ]],
      data_shared = data_h1_proxy_record_climate,
      unit_columns = c("window_id", "dataset_id")
    )
  ),
  # Why: Prove that every temporal H1 run used the climate block selected
  #   once for each region-by-age unit.
  targets::tar_target(
    name = "table_h1_proxy_shared_climate_check_slices",
    command = validate_shared_climate_selection_across_runs(
      data_used =
        list_h1_proxy_results_by_scale[["temporal_h1"]][[
          "table_h1_proxy_climate_used"
        ]],
      data_shared = data_h1_proxy_slice_climate,
      unit_columns = c("slice_set", "region", "age")
    )
  ),
  # Why: Prove that every common-cohort spatial H1 run had identical
  #   records.
  targets::tar_target(
    name = "table_h1_proxy_common_units_check_records",
    command = validate_human_proxy_common_units(
      data_run_units =
        list_h1_proxy_results_by_scale[["spatial_h1"]][[
          "data_h1_proxy_run_units"
        ]] |>
          dplyr::semi_join(
            dplyr::filter(
              data_h1_proxy_runs,
              .data[["age_window"]] == "common_cohort"
            ),
            by = "profile_id"
          ),
      unit_columns = "dataset_id"
    )
  ),
  # Why: Prove that every common-cohort temporal H1 run had identical
  #   region-by-age units.
  targets::tar_target(
    name = "table_h1_proxy_common_units_check_slices",
    command = validate_human_proxy_common_units(
      data_run_units =
        list_h1_proxy_results_by_scale[["temporal_h1"]][[
          "data_h1_proxy_run_units"
        ]] |>
          dplyr::semi_join(
            dplyr::filter(
              data_h1_proxy_runs,
              .data[["age_window"]] == "common_cohort"
            ),
            by = "profile_id"
          ),
      unit_columns = c("region", "age")
    )
  ),
  # Why: Define the comparison sets of the three main proxies, with KK10 and
  #   HYDE 3.5 land use at the SPD buffer or at 50 km (SPD always at its
  #   buffer), for each window and scale.
  targets::tar_target(
    name = "data_h1_proxy_comparison_runs",
    command = dplyr::bind_rows(
      spd_buffer = data_h1_proxy_runs |>
        dplyr::filter(
          .data[["human_proxy"]] %in%
            config_h1_proxy_outcomes[["main_proxies"]],
          .data[["proxy_support"]] == "spd_buffer"
        ),
      radius_50km = data_h1_proxy_runs |>
        dplyr::filter(
          .data[["human_proxy"]] %in%
            config_h1_proxy_outcomes[["main_proxies"]],
          (.data[["human_proxy"]] == "spd" &
            .data[["proxy_support"]] == "spd_buffer") |
            (.data[["human_proxy"]] != "spd" &
              .data[["proxy_support"]] == "radius_50km")
        ),
      .id = "gridded_support"
    ) |>
      dplyr::select(-dplyr::all_of("store"))
  ),
  # Why: Keep, per comparison set, window, and scale, the units estimable
  #   for all three proxies, so the proxy is the only thing that varies.
  targets::tar_target(
    name = "data_h1_proxy_comparison_units",
    command = data_h1_proxy_comparison_runs |>
      dplyr::distinct(
        .data[["gridded_support"]],
        .data[["age_window"]],
        .data[["analysis"]]
      ) |>
      dplyr::mutate(
        units = purrr::pmap(
          list(
            .data[["gridded_support"]],
            .data[["age_window"]],
            .data[["analysis"]]
          ),
          .f = ~ {
            vec_profiles <-
              data_h1_proxy_comparison_runs |>
              dplyr::filter(
                .data[["gridded_support"]] == ..1,
                .data[["age_window"]] == ..2,
                .data[["analysis"]] == ..3
              ) |>
              dplyr::pull("profile_id")

            vec_unit_columns <-
              if (
                identical(..3, "spatial_h1")
              ) {
                "dataset_id"
              } else {
                c("region", "age")
              }

            select_shared_estimable_units(
              data_units = list_h1_proxy_results_by_scale[[..3]][[
                "data_h1_proxy_estimable_units"
              ]] |>
                dplyr::filter(.data[["profile_id"]] %in% vec_profiles),
              unit_columns = vec_unit_columns,
              required_groups = vec_profiles
            )
          }
        ),
        n_units = purrr::map_int(.data[["units"]], nrow)
      )
  ),
  # Why: Refit the spatial H1 decision metric of each main proxy on the
  #   records estimable for all three proxies of its comparison set.
  targets::tar_target(
    name = "data_h1_proxy_comparison_metrics_spatial",
    command = data_h1_proxy_comparison_runs |>
      dplyr::filter(.data[["analysis"]] == "spatial_h1") |>
      dplyr::mutate(
        metric = purrr::pmap(
          list(
            .data[["gridded_support"]],
            .data[["age_window"]],
            .data[["profile_id"]]
          ),
          .f = ~ {
            data_shared <-
              data_h1_proxy_comparison_units |>
              dplyr::filter(
                .data[["gridded_support"]] == ..1,
                .data[["age_window"]] == ..2,
                .data[["analysis"]] == "spatial_h1"
              ) |>
              dplyr::pull("units") |>
              purrr::pluck(1)

            data_records <-
              list_h1_proxy_run_results[[..3]][[
                "data_h1_proxy_estimable_units"
              ]] |>
              dplyr::semi_join(data_shared, by = "dataset_id")

            fit_human_proxy_spatial_decision_metric(
              data_records = data_records,
              n_bootstrap = h1_proxy_config[["n_bootstrap"]],
              seed = h1_proxy_config[["seed"]],
              permutations = h1_proxy_config[["permutations"]],
              alpha = h1_proxy_config[["alpha"]],
              min_unique_locations =
                h1_proxy_config[["min_unique_locations"]],
              min_residual_df = h1_proxy_config[["min_spatial_residual_df"]],
              distance_km = h1_proxy_config[["spatial_distances_km"]]
            )
          }
        )
      )
  ),
  # Why: Recompute the temporal H1 decision metric of each main proxy on the
  #   units estimable for all three proxies of its comparison set.
  targets::tar_target(
    name = "data_h1_proxy_comparison_metrics_temporal",
    command = data_h1_proxy_comparison_runs |>
      dplyr::filter(.data[["analysis"]] == "temporal_h1") |>
      dplyr::mutate(
        metric = purrr::pmap(
          list(
            .data[["gridded_support"]],
            .data[["age_window"]],
            .data[["profile_id"]]
          ),
          .f = ~ {
            data_shared <-
              data_h1_proxy_comparison_units |>
              dplyr::filter(
                .data[["gridded_support"]] == ..1,
                .data[["age_window"]] == ..2,
                .data[["analysis"]] == "temporal_h1"
              ) |>
              dplyr::pull("units") |>
              purrr::pluck(1)

            data_compositions <-
              list_h1_proxy_run_results[[..3]][[
                "data_h1_proxy_estimable_units"
              ]] |>
              dplyr::semi_join(data_shared, by = c("region", "age"))

            summarise_human_proxy_temporal_decision_metric(
              data_composition = data_compositions,
              block_length = h1_proxy_config[["temporal_block_length"]],
              n_bootstrap = h1_proxy_config[["n_bootstrap"]],
              seed = h1_proxy_config[["seed"]]
            )
          }
        )
      )
  ),
  # Why: Publish the overall, regional, and stratum estimates of the
  #   shared-record spatial comparisons, used by the balance maps.
  targets::tar_target(
    name = "table_h1_proxy_comparison_balance_estimates",
    command = data_h1_proxy_comparison_metrics_spatial |>
      dplyr::mutate(
        metric = purrr::map(.data[["metric"]], "estimates")
      ) |>
      tidyr::unnest(cols = dplyr::all_of("metric"))
  ),
  # Why: Publish the units shared by the three main proxies in each
  #   comparison set, window, and scale (records or region-by-age units).
  targets::tar_target(
    name = "table_h1_proxy_comparison_units",
    command = data_h1_proxy_comparison_units |>
      dplyr::select(
        dplyr::all_of(c("gridded_support", "age_window", "analysis", "units"))
      ) |>
      tidyr::unnest(cols = dplyr::all_of("units"))
  ),
  # Why: Assemble every decision metric in one long table over window,
  #   record set (shared by the three main proxies, or all records of the
  #   run), and support of the gridded proxies.
  targets::tar_target(
    name = "table_h1_proxy_outcome_metrics",
    command = dplyr::bind_rows(
      data_h1_proxy_comparison_metrics_spatial |>
        dplyr::mutate(
          metric = purrr::map(.data[["metric"]], "metric")
        ),
      data_h1_proxy_comparison_metrics_temporal |>
        dplyr::mutate(
          metric = purrr::map(.data[["metric"]], "metric")
        )
    ) |>
      tidyr::unnest(cols = dplyr::all_of("metric")) |>
      dplyr::mutate(
        comparison = "main_proxies",
        record_set = "shared_records"
      ) |>
      dplyr::bind_rows(
        dplyr::bind_rows(
          list_h1_proxy_results_by_scale[["spatial_h1"]][[
            "table_h1_proxy_decision_metric"
          ]],
          list_h1_proxy_results_by_scale[["temporal_h1"]][[
            "table_h1_proxy_decision_metric"
          ]]
        ) |>
          dplyr::inner_join(
            config_h1_proxy_outcomes[["support_map"]],
            by = c("human_proxy", "proxy_support"),
            relationship = "many-to-many"
          ) |>
          dplyr::mutate(record_set = "all_records")
      ) |>
      dplyr::transmute(
        analysis = .data[["analysis"]],
        comparison = .data[["comparison"]],
        gridded_support = .data[["gridded_support"]],
        age_window = .data[["age_window"]],
        record_set = .data[["record_set"]],
        profile_id = .data[["profile_id"]],
        human_proxy = .data[["human_proxy"]],
        proxy_support = .data[["proxy_support"]],
        profile = .data[["profile"]],
        estimate = dplyr::coalesce(
          .data[["adjusted_balance"]],
          .data[["pooled_difference"]]
        ),
        lwr_95 = .data[["lwr_95"]],
        upr_95 = .data[["upr_95"]],
        n_units = dplyr::coalesce(
          .data[["n_records"]],
          .data[["n_units"]]
        ),
        n_human_greater = dplyr::coalesce(
          .data[["n_human_dominant"]],
          .data[["n_human_greater"]]
        ),
        mean_difference = .data[["mean_difference"]]
      ) |>
      dplyr::arrange(
        .data[["analysis"]],
        .data[["comparison"]],
        .data[["human_proxy"]],
        .data[["gridded_support"]],
        .data[["age_window"]],
        .data[["record_set"]],
        .data[["profile"]]
      )
  ),
  # Why: Classify every metric and build the full outcome table (decision
  #   record D5 and D6): one row per scale and proxy, one column per support,
  #   window, and record set.
  targets::tar_target(
    name = "output_h1_proxy_outcome_table",
    command = summarise_human_proxy_outcome_table(
      data_metrics = table_h1_proxy_outcome_metrics,
      headline_profile = "zero_truncated",
      row_columns = c("analysis", "comparison", "human_proxy"),
      table_columns = c("gridded_support", "age_window", "record_set"),
      group_columns = c("analysis", "comparison")
    )
  ),
  # Why: Publish the classified metrics with their headline classes.
  targets::tar_target(
    name = "table_h1_proxy_outcome_summary",
    command = output_h1_proxy_outcome_table[["summary"]]
  ),
  # Why: Publish the outcome table: one class per proxy and per support,
  #   window, and record set.
  targets::tar_target(
    name = "table_h1_proxy_outcome_table",
    command = output_h1_proxy_outcome_table[["table"]]
  ),
  # Why: Track the reference record balances of the matched SPD bridge.
  targets::tar_target(
    name = "file_h1_proxy_reference_records",
    command = file.path(
      path_reference_tables,
      "decision_spatial_common_cohort.csv"
    ),
    format = "file"
  ),
  # Why: Track the reference model statuses with their residual df.
  targets::tar_target(
    name = "file_h1_proxy_reference_status",
    command = file.path(path_reference_tables, "temporal_status.csv"),
    format = "file"
  ),
  # Why: Read the reference record balances of the matched SPD bridge.
  targets::tar_target(
    name = "data_h1_proxy_reference_records",
    command = readr::read_csv(
      file_h1_proxy_reference_records,
      col_types = readr::cols(dataset_id = readr::col_character())
    ) |>
      dplyr::filter(
        .data[["model_id"]] ==
          config_h1_proxy_outcomes[["reference_model_id"]]
      )
  ),
  # Why: Read the reference statuses, whose residual df explain records the
  #   new residual-df minimum removes.
  targets::tar_target(
    name = "data_h1_proxy_reference_status",
    command = readr::read_csv(
      file_h1_proxy_reference_status,
      col_types = readr::cols(dataset_id = readr::col_character())
    ) |>
      dplyr::filter(
        .data[["model_id"]] ==
          config_h1_proxy_outcomes[["reference_model_id"]]
      )
  ),
  # Why: Fingerprint the reference designs so a changed reference store
  #   invalidates the code check.
  targets::tar_target(
    name = "fingerprint_h1_proxy_reference_designs",
    command = compute_target_store_fingerprint(
      store = store_reference,
      target_names = "data_colmanaged_temporal_designs",
      runner = runner_reference
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  # Why: Import the reference record histories (its own PAP version) of the
  #   reference cohort, without its climate selection.
  targets::tar_target(
    name = "data_h1_proxy_reference_units",
    command = {
      fingerprint_h1_proxy_reference_designs

      load_target_store_value(
        store = store_reference,
        target_name = "data_colmanaged_temporal_designs",
        runner = runner_reference
      ) |>
        dplyr::filter(
          .data[["model_id"]] ==
            config_h1_proxy_outcomes[["reference_model_id"]]
        ) |>
        dplyr::transmute(
          dataset_id = as.character(.data[["dataset_id"]]),
          data_merge = .data[["data_merge"]]
        ) |>
        dplyr::semi_join(data_h1_proxy_reference_records, by = "dataset_id")
    }
  ),
  # Why: Select climate for the reference histories with the new code.
  targets::tar_target(
    name = "data_h1_proxy_reference_climate",
    command = select_shared_climate_predictors(
      data_units = data_h1_proxy_reference_units,
      unit_columns = "dataset_id",
      climate_candidates = h1_proxy_config[["climate_candidates"]],
      max_cor = h1_proxy_config[["max_cor"]],
      max_vif = h1_proxy_config[["max_vif"]]
    )
  ),
  # Why: Build the reference SPD designs with the new code.
  targets::tar_target(
    name = "data_h1_proxy_reference_designs",
    command = prepare_shared_climate_hvarpart_designs(
      data_units = data_h1_proxy_reference_units,
      data_climate_selection = data_h1_proxy_reference_climate,
      model_specifications = tibble::tibble(
        model_id = config_h1_proxy_outcomes[["reference_model_id"]],
        model_label = "Reference SPD model refitted with the new code",
        human_candidates = list(
          config_h1_proxy_outcomes[["reference_human_column"]]
        ),
        human_preference = list(
          config_h1_proxy_outcomes[["reference_human_column"]]
        ),
        exploratory = FALSE
      ),
      unit_columns = "dataset_id",
      max_cor = h1_proxy_config[["max_cor"]],
      max_vif = h1_proxy_config[["max_vif"]]
    )
  ),
  # Why: Refit the reference SPD model with the new code and the reference
  #   residual-df minimum.
  targets::tar_target(
    name = "output_h1_proxy_reference_fits",
    command = fit_local_temporal_hvarpart_models(
      data_designs = data_h1_proxy_reference_designs,
      response_vars = h1_proxy_response_variables,
      seed = h1_proxy_config[["seed"]],
      min_unique_ages = h1_proxy_config[["min_unique_ages"]],
      min_residual_df =
        config_h1_proxy_outcomes[["reference_min_residual_df"]],
      distance_years = h1_proxy_config[["temporal_distances_years"]],
      permutations = h1_proxy_config[["permutations"]]
    )
  ),
  # Why: Stop on any caught error of the reference refit.
  targets::tar_target(
    name = "check_h1_proxy_reference_fit_errors",
    command = output_h1_proxy_reference_fits |>
      dplyr::filter(!is.na(.data[["error_message"]])) |>
      nrow() |>
      magrittr::equals(0L) |>
      assertthat::assert_that(
        msg = "Reference refit errors occurred; see the reference fits."
      )
  ),
  # Why: Extract statuses and contributions of the reference refit.
  targets::tar_target(
    name = "result_h1_proxy_reference_refit",
    command = {
      check_h1_proxy_reference_fit_errors

      summarise_local_temporal_hvarpart_models(
        data_results = output_h1_proxy_reference_fits,
        analysis_prefix = "spatial_h1_reference_refit"
      )
    }
  ),
  # Why: Convert the reference refit to estimable record balances.
  targets::tar_target(
    name = "data_h1_proxy_reference_refit_records",
    command = prepare_time_controlled_importance_records(
      data_components = result_h1_proxy_reference_refit[["components"]],
      data_status = result_h1_proxy_reference_refit[["status"]],
      data_meta = data_h1_proxy_meta
    ) |>
      filter_estimable_balance_records()
  ),
  # Why: Check the code: the refit of the reference inputs must reproduce
  #   every reference record (acceptance criterion).
  targets::tar_target(
    name = "output_h1_proxy_code_reproduction",
    command = diagnose_human_proxy_reference_reproduction(
      data_records = data_h1_proxy_reference_refit_records,
      data_status = result_h1_proxy_reference_refit[["status"]],
      data_reference = data_h1_proxy_reference_records,
      data_reference_status = data_h1_proxy_reference_status,
      data_cohort = data_h1_proxy_reference_records |>
        dplyr::select(dplyr::all_of("dataset_id")),
      min_residual_df =
        config_h1_proxy_outcomes[["reference_min_residual_df"]],
      tolerance = config_h1_proxy_outcomes[["reproduction_tolerance"]]
    )
  ),
  # Why: Publish the one-row summary of the code check.
  targets::tar_target(
    name = "table_h1_proxy_code_reproduction_summary",
    command = output_h1_proxy_code_reproduction[["summary"]]
  ),
  # Why: Report how the current PAPs change the reference SPD result: the SPD
  #   common-cohort run against the reference, record by record.
  targets::tar_target(
    name = "output_h1_proxy_pap_update_comparison",
    command = diagnose_human_proxy_reference_reproduction(
      data_records = list_h1_proxy_run_results[[
        config_h1_proxy_outcomes[["reproduction_profile_id"]]
      ]][["data_h1_proxy_estimable_units"]],
      data_status = list_h1_proxy_run_results[[
        config_h1_proxy_outcomes[["reproduction_profile_id"]]
      ]][["table_h1_proxy_status"]],
      data_reference = data_h1_proxy_reference_records,
      data_reference_status = data_h1_proxy_reference_status,
      data_cohort = data_h1_proxy_common_cohort,
      min_residual_df = h1_proxy_config[["min_temporal_residual_df"]],
      tolerance = config_h1_proxy_outcomes[["reproduction_tolerance"]]
    )
  ),
  # Why: Publish the one-row summary of the PAP-update comparison.
  targets::tar_target(
    name = "table_h1_proxy_pap_update_summary",
    command = output_h1_proxy_pap_update_comparison[["summary"]]
  ),
  # Why: Export the outcome tables, checks, statuses, and reproduction.
  targets::tar_target(
    name = "files_h1_proxy_outcome_tables",
    command = {
      list_tables <-
        list(
          human_proxies__outcome_table = table_h1_proxy_outcome_table,
          human_proxies__outcome_summary = table_h1_proxy_outcome_summary,
          human_proxies__outcome_metrics =
            output_h1_proxy_outcome_table[["long"]],
          human_proxies__comparison_units =
            data_h1_proxy_comparison_units |>
              dplyr::select(
                dplyr::all_of(c(
                  "gridded_support", "age_window", "analysis", "n_units"
                ))
              ),
          human_proxies__comparison_balance_estimates__time_and_space_control =
            table_h1_proxy_comparison_balance_estimates,
          human_proxies__shared_climate_check__time_control =
            table_h1_proxy_shared_climate_check_records,
          human_proxies__shared_climate_check__space_control =
            table_h1_proxy_shared_climate_check_slices,
          human_proxies__common_units_check__time_control =
            table_h1_proxy_common_units_check_records,
          human_proxies__common_units_check__space_control =
            table_h1_proxy_common_units_check_slices,
          human_proxies__model_status__time_control =
            list_h1_proxy_results_by_scale[["spatial_h1"]][[
              "table_h1_proxy_status"
            ]],
          human_proxies__model_status__space_control =
            list_h1_proxy_results_by_scale[["temporal_h1"]][[
              "table_h1_proxy_status"
            ]],
          human_proxies__reference_code_check__records =
            output_h1_proxy_code_reproduction[["records"]],
          human_proxies__reference_code_check__summary =
            table_h1_proxy_code_reproduction_summary,
          human_proxies__pap_update_comparison__records =
            output_h1_proxy_pap_update_comparison[["records"]],
          human_proxies__pap_update_comparison__summary =
            table_h1_proxy_pap_update_summary,
          human_proxies__outcomes__provenance = tibble::tibble(
            profile_id = names(fingerprint_h1_proxy_runs),
            fingerprint = unname(fingerprint_h1_proxy_runs)
          ) |>
            dplyr::bind_rows(
              tibble::tibble(
                profile_id = "inputs",
                fingerprint = fingerprint_h1_proxy_inputs
              )
            )
        )

      vec_paths <-
        file.path(path_tables, stringr::str_c(names(list_tables), ".csv")) |>
        rlang::set_names(names(list_tables))

      save_collinearity_managed_hvarpart_tables(list_tables, vec_paths)
    },
    format = "file"
  ),
  # Why: Stop, after the tables are written, when the new code does not
  #   reproduce the reference SPD result from the reference inputs.
  targets::tar_target(
    name = "check_h1_proxy_code_reproduction",
    command = {
      files_h1_proxy_outcome_tables

      assertthat::assert_that(
        isTRUE(
          dplyr::pull(
            table_h1_proxy_code_reproduction_summary,
            "is_reproduced"
          )
        ),
        msg = "The new code does not reproduce the reference SPD result."
      )
    }
  )
)
