#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#       Reviewer spatial-dependence sensitivity for H1
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)

response_vars_h1 <-
  c(
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "roc",
    "dcca_axis_1",
    "density_diversity",
    "density_turnover"
  )
predictor_vars_h1 <-
  list(
    human = "spd",
    climate = c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    )
  )
predictor_vars_events_h1 <-
  list(
    human = c(
      "fi",
      "fc",
      "ec",
      "cc",
      "es",
      "ei",
      "weak",
      "medium",
      "strong"
    ),
    climate = predictor_vars_h1[["climate"]]
  )
predictor_columns_h1 <-
  unique(
    c(
      unlist(predictor_vars_h1, use.names = FALSE),
      unlist(predictor_vars_events_h1, use.names = FALSE)
    )
  )

#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

list(
  targets::tar_target(
    name = spatial_sensitivity_config,
    command = list(
      seed = as.integer(set_seed),
      thinning_distances_km = c(250, 500),
      thinning_repetitions = 100L,
      permutations = 999L,
      alpha = 0.05,
      min_unique_locations = 20L,
      min_residual_df = 10L,
      min_unique_ages = 10L,
      min_temporal_residual_df = 5L,
      temporal_distances_years = c(500, 1000)
    )
  ),
  targets::tar_target(
    name = data_meta_path,
    command = file.path(
      data_storage_path,
      "Assembly",
      RUtilpol::get_latest_file_name(
        file_name = "data_meta",
        dir = paste0(data_storage_path, "Assembly/")
      )
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_meta,
    command = resolve_file_path(data_meta_path)
  ),
  targets::tar_target(
    name = data_properties_filtered_path,
    command = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_paps",
      "objects",
      "data_properties_filtered"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_properties_filtered,
    command = resolve_file_path(data_properties_filtered_path)
  ),
  targets::tar_target(
    name = data_properties_path,
    command = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_paps",
      "objects",
      "data_properties"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_properties,
    command = resolve_file_path(data_properties_path)
  ),
  targets::tar_target(
    name = data_predictors_filtered_path,
    command = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_predictors",
      "objects",
      "data_predictors_filtered"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_predictors_filtered,
    command = resolve_file_path(data_predictors_filtered_path)
  ),
  targets::tar_target(
    name = data_predictors_path,
    command = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_predictors",
      "objects",
      "data_predictors"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_predictors,
    command = resolve_file_path(data_predictors_path)
  ),
  targets::tar_target(
    name = data_hvar_filtered,
    command = prepare_combined_data(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  targets::tar_target(
    name = data_properties_temporal,
    command = prepare_filtered_hvarpart_data(
      data_source = data_properties,
      data_meta = data_meta,
      age_from = 0,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  targets::tar_target(
    name = data_predictors_temporal,
    command = prepare_filtered_hvarpart_data(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = 0,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  targets::tar_target(
    name = data_hvar_temporal,
    command = prepare_combined_data(
      data_source_properties = data_properties_temporal,
      data_source_predictors = data_predictors_temporal
    )
  ),
  targets::tar_target(
    name = output_dataset_age_collapse_filtered,
    command = aggregate_hvar_dataset_ages(
      data_source = data_hvar_filtered,
      response_vars = response_vars_h1,
      predictor_vars = predictor_columns_h1
    )
  ),
  targets::tar_target(
    name = data_hvar_filtered_unique_age,
    command = output_dataset_age_collapse_filtered[["data"]]
  ),
  targets::tar_target(
    name = table_dataset_age_collapse_filtered,
    command = output_dataset_age_collapse_filtered[["audit"]]
  ),
  targets::tar_target(
    name = output_dataset_age_collapse_temporal,
    command = aggregate_hvar_dataset_ages(
      data_source = data_hvar_temporal,
      response_vars = response_vars_h1,
      predictor_vars = predictor_columns_h1
    )
  ),
  targets::tar_target(
    name = data_hvar_temporal_unique_age,
    command = output_dataset_age_collapse_temporal[["data"]]
  ),
  targets::tar_target(
    name = table_dataset_age_collapse_temporal,
    command = output_dataset_age_collapse_temporal[["audit"]]
  ),
  targets::tar_target(
    name = data_hvar_timebins_unique_age,
    command = prepare_hvarpart_timebin_data(
      data_source = data_hvar_temporal_unique_age,
      data_meta = data_meta
    )
  ),
  targets::tar_target(
    name = data_hvar_timebins_spd_unique_age,
    command = data_hvar_timebins_unique_age |>
      dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500))
  ),
  targets::tar_target(
    name = data_hvar_timebins_spd,
    command = prepare_hvarpart_timebin_data(
      data_source = data_hvar_temporal,
      data_meta = data_meta
    ) |>
      dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500))
  ),
  targets::tar_target(
    name = output_spatial_spd_sensitivity,
    command = fit_hvarpart_models(
      data_source = data_hvar_filtered,
      response_vars = response_vars_h1,
      predictor_vars = predictor_vars_h1,
      response_dist = NULL,
      data_response_dist = NULL,
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = spatial_sensitivity_config[["permutations"]],
      fail_on_error = FALSE
    )
  ),
  targets::tar_target(
    name = data_spatial_importance_sensitivity,
    command = compute_hvarpart_importance(
      data_source = output_spatial_spd_sensitivity |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              dplyr::all_of(c(
                "dataset_id",
                "region",
                "climatezone"
              ))
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_spd_sensitivity"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    )
  ),
  targets::tar_target(
    name = data_spatial_importance_records,
    command = prepare_spatial_importance_records(
      data_importance = data_spatial_importance_sensitivity,
      data_meta = data_meta
    )
  ),
  targets::tar_target(
    name = output_spatial_importance_filter,
    command = fit_spatial_importance(
      data_records = data_spatial_importance_records,
      permutations = spatial_sensitivity_config[["permutations"]],
      alpha = spatial_sensitivity_config[["alpha"]],
      min_unique_locations =
        spatial_sensitivity_config[["min_unique_locations"]],
      min_residual_df =
        spatial_sensitivity_config[["min_residual_df"]],
      distance_km =
        spatial_sensitivity_config[["thinning_distances_km"]],
      seed = spatial_sensitivity_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = data_spatial_importance_thinning,
    command = select_spatial_thinning(
      data_source = data_spatial_importance_records,
      strata = c("region", "climatezone"),
      distance_km =
        spatial_sensitivity_config[["thinning_distances_km"]],
      repetitions =
        spatial_sensitivity_config[["thinning_repetitions"]],
      id_col = "model_id",
      seed = spatial_sensitivity_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_importance_sensitivity,
    command = summarise_spatial_importance_sensitivity(
      data_records = data_spatial_importance_records,
      data_thinning = data_spatial_importance_thinning
    )
  ),
  targets::tar_target(
    name = table_spatial_importance_estimates,
    command = output_spatial_importance_filter[["estimates"]]
  ),
  targets::tar_target(
    name = table_spatial_importance_moran,
    command = output_spatial_importance_filter[["moran_diagnostics"]]
  ),
  targets::tar_target(
    name = table_spatial_importance_dbmem_diagnostics,
    command = output_spatial_importance_filter[["dbmem"]][["diagnostics"]]
  ),
  targets::tar_target(
    name = table_spatial_importance_dbmem_selection,
    command = tibble::tibble(
      status = output_spatial_importance_filter[["selection"]][["status"]],
      n_complete =
        output_spatial_importance_filter[["selection"]][["n_complete"]],
      n_candidates =
        output_spatial_importance_filter[["selection"]][["n_candidates"]],
      global_p_value =
        output_spatial_importance_filter[["selection"]][["global_p_value"]],
      full_adjusted_r_squared = purrr::pluck(
        output_spatial_importance_filter,
        "selection",
        "full_adjusted_r_squared"
      ),
      selected_names = stringr::str_c(
        output_spatial_importance_filter[["selection"]][["selected_names"]],
        collapse = ";"
      )
    )
  ),
  targets::tar_target(
    name = table_spatial_importance_robustness,
    command = classify_spatial_robustness(
      data_sensitivity = table_spatial_importance_sensitivity,
      data_spatial_estimates = table_spatial_importance_estimates
    ) |>
      dplyr::left_join(
        table_spatial_importance_moran |>
          dplyr::filter(
            .data[["spatial_scope"]] == "global",
            .data[["stage"]] == "residual"
          ) |>
          dplyr::group_by(.data[["profile"]]) |>
          dplyr::summarise(
            residual_spatial_dependence = any(
              .data[["positive_autocorrelation"]]
            ),
            .groups = "drop"
          ),
        by = "profile"
      )
  ),
  targets::tar_target(
    name = data_hvar_timebin_groups,
    command = prepare_hvarpart_timebin_groups(
      data_source = data_hvar_timebins_spd
    ),
    iteration = "list"
  ),
  targets::tar_target(
    name = output_temporal_spatial_group,
    command = list(
      region = data_hvar_timebin_groups[["region"]][1],
      age = data_hvar_timebin_groups[["age"]][1],
      result = fit_spatial_hvarpart_group(
        data_group = data_hvar_timebin_groups[["data_merge"]][[1]],
        response_vars = response_vars_h1,
        predictor_vars = predictor_vars_h1,
        permutations = spatial_sensitivity_config[["permutations"]],
        alpha = spatial_sensitivity_config[["alpha"]],
        min_unique_locations =
          spatial_sensitivity_config[["min_unique_locations"]],
        min_residual_df =
          spatial_sensitivity_config[["min_residual_df"]],
        distance_km =
          spatial_sensitivity_config[["thinning_distances_km"]],
        seed = spatial_sensitivity_config[["seed"]] +
          as.integer(data_hvar_timebin_groups[["age"]][1])
      )
    ),
    pattern = map(data_hvar_timebin_groups),
    iteration = "list"
  ),
  targets::tar_target(
    name = table_temporal_spatial_status,
    command = output_temporal_spatial_group |>
      purrr::map_dfr(
        .f = ~ tibble::tibble(
          region = .x[["region"]],
          age = .x[["age"]],
          status = .x[["result"]][["status"]],
          n_samples = .x[["result"]][["n_samples"]],
          selection_status =
            .x[["result"]][["selection"]][["status"]],
          n_candidates =
            .x[["result"]][["selection"]][["n_candidates"]],
          n_selected = length(
            .x[["result"]][["selection"]][["selected_names"]]
          ),
          global_p_value =
            .x[["result"]][["selection"]][["global_p_value"]],
          spatial_adjusted_r_squared =
            .x[["result"]][["selection"]][["full_adjusted_r_squared"]]
        )
      )
  ),
  targets::tar_target(
    name = table_temporal_unique_adjusted_r2,
    command = output_temporal_spatial_group |>
      purrr::map_dfr(
        .f = ~ .x[["result"]][["unique_adjusted_r2"]] |>
          dplyr::mutate(
            region = .x[["region"]],
            age = .x[["age"]],
            .before = 1L
          )
      )
  ),
  targets::tar_target(
    name = table_temporal_residual_moran,
    command = output_temporal_spatial_group |>
      purrr::map_dfr(
        .f = ~ .x[["result"]][["residual_moran"]] |>
          dplyr::mutate(
            region = .x[["region"]],
            age = .x[["age"]],
            .before = 1L
          )
      )
  ),
  targets::tar_target(
    name = table_temporal_remaining_spatial_test,
    command = output_temporal_spatial_group |>
      purrr::map_dfr(
        .f = ~ .x[["result"]][["remaining_spatial_test"]] |>
          dplyr::mutate(
            region = .x[["region"]],
            age = .x[["age"]],
            .before = 1L
          )
      )
  ),
  targets::tar_target(
    name = table_temporal_hvarpart_components,
    command = output_temporal_spatial_group |>
      purrr::map_dfr(
        .f = ~ {
          data_human_climate_only <-
            .x[["result"]][["human_climate_only_hvarpart"]][["summary_table"]] |>
            dplyr::mutate(model_profile = "human_climate")
          data_spatial <-
            if (
              is.null(.x[["result"]][["spatial_hvarpart"]])
            ) {
              tibble::tibble()
            } else {
              .x[["result"]][["spatial_hvarpart"]][["summary_table"]] |>
                dplyr::mutate(model_profile = "human_climate_space")
            }
          dplyr::bind_rows(data_human_climate_only, data_spatial) |>
            dplyr::mutate(
              region = .x[["region"]],
              age = .x[["age"]],
              .before = 1L
            )
        }
      )
  ),
  targets::tar_target(
    name = table_temporal_hvarpart_rankings,
    command = table_temporal_hvarpart_components |>
      dplyr::filter(.data[["predictor"]] %in% c("human", "climate")) |>
      dplyr::select(
        dplyr::all_of(c(
          "region",
          "age",
          "model_profile",
          "predictor",
          "Individual"
        ))
      ) |>
      tidyr::pivot_wider(
        names_from = c("model_profile", "predictor"),
        values_from = "Individual"
      ) |>
      dplyr::mutate(
        human_climate_only_balance =
          .data[["human_climate_human"]] -
          .data[["human_climate_climate"]],
        spatial_balance = dplyr::coalesce(
          .data[["human_climate_space_human"]] -
            .data[["human_climate_space_climate"]],
          .data[["human_climate_only_balance"]]
        ),
        human_climate_only_ranking = dplyr::case_when(
          .data[["human_climate_only_balance"]] > 0 ~ "human",
          .data[["human_climate_only_balance"]] < 0 ~ "climate",
          .default = "tie"
        ),
        spatial_ranking = dplyr::case_when(
          .data[["spatial_balance"]] > 0 ~ "human",
          .data[["spatial_balance"]] < 0 ~ "climate",
          .default = "tie"
        ),
        ranking_changed =
          .data[["human_climate_only_ranking"]] != .data[["spatial_ranking"]]
      ) |>
      dplyr::left_join(
        table_temporal_spatial_status |>
          dplyr::select(
            dplyr::all_of(c(
              "region",
              "age",
              "status",
              "n_selected"
            ))
          ),
        by = c("region", "age")
      )
  ),
  targets::tar_target(
    name = output_time_controlled_hvarpart_spd,
    command = fit_temporal_hvarpart_datasets(
      data_source = data_hvar_filtered_unique_age,
      response_vars = response_vars_h1,
      predictor_vars = predictor_vars_h1,
      min_unique_ages =
        spatial_sensitivity_config[["min_unique_ages"]],
      min_residual_df =
        spatial_sensitivity_config[["min_temporal_residual_df"]],
      distance_years =
        spatial_sensitivity_config[["temporal_distances_years"]],
      permutations = spatial_sensitivity_config[["permutations"]],
      seed = spatial_sensitivity_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = output_time_controlled_hvarpart_events,
    command = fit_temporal_hvarpart_datasets(
      data_source = data_hvar_filtered_unique_age,
      response_vars = response_vars_h1,
      predictor_vars = predictor_vars_events_h1,
      min_unique_ages =
        spatial_sensitivity_config[["min_unique_ages"]],
      min_residual_df =
        spatial_sensitivity_config[["min_temporal_residual_df"]],
      distance_years =
        spatial_sensitivity_config[["temporal_distances_years"]],
      permutations = spatial_sensitivity_config[["permutations"]],
      seed = spatial_sensitivity_config[["seed"]] + 10000L
    )
  ),
  targets::tar_target(
    name = result_time_controlled_hvarpart_spd,
    command = summarise_temporal_hvarpart_results(
      data_results = output_time_controlled_hvarpart_spd,
      analysis = "spatial_spd"
    )
  ),
  targets::tar_target(
    name = result_time_controlled_hvarpart_events,
    command = summarise_temporal_hvarpart_results(
      data_results = output_time_controlled_hvarpart_events,
      analysis = "spatial_events"
    )
  ),
  targets::tar_target(
    name = table_time_control_status,
    command = dplyr::bind_rows(
      result_time_controlled_hvarpart_spd[["status"]],
      result_time_controlled_hvarpart_events[["status"]]
    )
  ),
  targets::tar_target(
    name = table_time_control_hierarchical_contributions,
    command = dplyr::bind_rows(
      result_time_controlled_hvarpart_spd[["components"]],
      result_time_controlled_hvarpart_events[["components"]]
    )
  ),
  targets::tar_target(
    name = table_time_control_unique_adjusted_r2,
    command = dplyr::bind_rows(
      result_time_controlled_hvarpart_spd[["unique_adjusted_r2"]],
      result_time_controlled_hvarpart_events[["unique_adjusted_r2"]]
    )
  ),
  targets::tar_target(
    name = table_time_control_residual_moran,
    command = dplyr::bind_rows(
      result_time_controlled_hvarpart_spd[["residual_moran"]],
      result_time_controlled_hvarpart_events[["residual_moran"]]
    )
  ),
  targets::tar_target(
    name = data_time_controlled_balance_records_all,
    command = prepare_time_controlled_importance_records(
      data_components = table_time_control_hierarchical_contributions,
      data_status = table_time_control_status,
      data_meta = data_meta
    )
  ),
  targets::tar_target(
    name = data_time_controlled_balance_records,
    command = data_time_controlled_balance_records_all |>
      dplyr::filter(
        .data[["analysis"]] == "spatial_spd",
        is.finite(.data[["signed_balance"]]),
        .data[["signed_weight"]] > 0,
        is.finite(.data[["zero_balance"]]),
        .data[["zero_weight"]] > 0
      )
  ),
  targets::tar_target(
    name = data_human_climate_only_matched_records,
    command = prepare_human_climate_only_records(
      data_records = data_time_controlled_balance_records
    )
  ),
  targets::tar_target(
    name = table_human_climate_only_matched_estimates,
    command = summarise_spatial_importance_subset(
      data_subset = data_human_climate_only_matched_records,
      sensitivity_type = "human_climate_only"
    ) |>
      dplyr::mutate(
        ranking = dplyr::case_when(
          .data[["importance_balance"]] > 0 ~ "human",
          .data[["importance_balance"]] < 0 ~ "climate",
          .default = "tie"
        )
      )
  ),
  targets::tar_target(
    name = output_spatiotemporal_balance_filter,
    command = fit_spatial_importance(
      data_records = data_time_controlled_balance_records,
      permutations = spatial_sensitivity_config[["permutations"]],
      alpha = spatial_sensitivity_config[["alpha"]],
      min_unique_locations =
        spatial_sensitivity_config[["min_unique_locations"]],
      min_residual_df =
        spatial_sensitivity_config[["min_residual_df"]],
      distance_km =
        spatial_sensitivity_config[["thinning_distances_km"]],
      seed = spatial_sensitivity_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = data_spatiotemporal_balance_thinning,
    command = select_spatial_thinning(
      data_source = data_time_controlled_balance_records,
      strata = c("region", "climatezone"),
      distance_km =
        spatial_sensitivity_config[["thinning_distances_km"]],
      repetitions =
        spatial_sensitivity_config[["thinning_repetitions"]],
      id_col = "model_id",
      seed = spatial_sensitivity_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_sensitivity,
    command = summarise_spatial_importance_sensitivity(
      data_records = data_time_controlled_balance_records,
      data_thinning = data_spatiotemporal_balance_thinning
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
      status =
        output_spatiotemporal_balance_filter[["selection"]][["status"]],
      n_complete =
        output_spatiotemporal_balance_filter[["selection"]][[
          "n_complete"
        ]],
      n_candidates =
        output_spatiotemporal_balance_filter[["selection"]][[
          "n_candidates"
        ]],
      global_p_value =
        output_spatiotemporal_balance_filter[["selection"]][[
          "global_p_value"
        ]],
      full_adjusted_r_squared =
        output_spatiotemporal_balance_filter[["selection"]][[
          "full_adjusted_r_squared"
        ]],
      selected_terms = stringr::str_c(
        output_spatiotemporal_balance_filter[["selection"]][[
          "selected_names"
        ]],
        collapse = ";"
      )
    )
  ),
  targets::tar_target(
    name = table_spatiotemporal_balance_robustness,
    command = classify_spatiotemporal_robustness(
      data_sensitivity = table_spatiotemporal_balance_sensitivity,
      data_spatial_estimates = table_spatiotemporal_balance_estimates,
      data_human_climate_only = table_human_climate_only_matched_estimates
    )
  ),
  targets::tar_target(
    name = output_spatial_controlled_hvarpart_spd,
    command = fit_spatial_hvarpart_dataset(
      data_source = data_hvar_timebins_spd_unique_age,
      analysis = "temporal_spd",
      response_vars = response_vars_h1,
      predictor_vars = predictor_vars_h1,
      permutations = spatial_sensitivity_config[["permutations"]],
      alpha = spatial_sensitivity_config[["alpha"]],
      min_unique_locations =
        spatial_sensitivity_config[["min_unique_locations"]],
      min_residual_df =
        spatial_sensitivity_config[["min_residual_df"]],
      distance_km =
        spatial_sensitivity_config[["thinning_distances_km"]],
      seed = spatial_sensitivity_config[["seed"]]
    )
  ),
  targets::tar_target(
    name = output_spatial_controlled_hvarpart_events,
    command = fit_spatial_hvarpart_dataset(
      data_source = data_hvar_timebins_unique_age,
      analysis = "temporal_events",
      response_vars = response_vars_h1,
      predictor_vars = predictor_vars_events_h1,
      permutations = spatial_sensitivity_config[["permutations"]],
      alpha = spatial_sensitivity_config[["alpha"]],
      min_unique_locations =
        spatial_sensitivity_config[["min_unique_locations"]],
      min_residual_df =
        spatial_sensitivity_config[["min_residual_df"]],
      distance_km =
        spatial_sensitivity_config[["thinning_distances_km"]],
      seed = spatial_sensitivity_config[["seed"]] + 10000L
    )
  ),
  targets::tar_target(
    name = result_spatial_controlled_hvarpart_spd,
    command = summarise_spatial_hvarpart_results(
      data_results = output_spatial_controlled_hvarpart_spd
    )
  ),
  targets::tar_target(
    name = result_spatial_controlled_hvarpart_events,
    command = summarise_spatial_hvarpart_results(
      data_results = output_spatial_controlled_hvarpart_events
    )
  ),
  targets::tar_target(
    name = table_spatial_control_status,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["status"]],
      result_spatial_controlled_hvarpart_events[["status"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_dbmem_selection,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["selection"]],
      result_spatial_controlled_hvarpart_events[["selection"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_dbmem_diagnostics,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["dbmem_diagnostics"]],
      result_spatial_controlled_hvarpart_events[["dbmem_diagnostics"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_hierarchical_contributions,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["components"]],
      result_spatial_controlled_hvarpart_events[["components"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_unique_adjusted_r2,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["unique_adjusted_r2"]],
      result_spatial_controlled_hvarpart_events[["unique_adjusted_r2"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_residual_moran,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["residual_moran"]],
      result_spatial_controlled_hvarpart_events[["residual_moran"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_remaining_signal,
    command = dplyr::bind_rows(
      result_spatial_controlled_hvarpart_spd[["remaining_spatial_test"]],
      result_spatial_controlled_hvarpart_events[["remaining_spatial_test"]]
    )
  ),
  targets::tar_target(
    name = table_spatial_control_zero_truncated_composition,
    command = prepare_spatial_hvarpart_composition(
      data_components = table_spatial_control_hierarchical_contributions,
      data_status = table_spatial_control_status
    )
  ),
  targets::tar_target(
    name = table_spatial_control_rankings,
    command = diagnose_spatial_hvarpart_rankings(
      data_components = table_spatial_control_hierarchical_contributions,
      data_status = table_spatial_control_status
    )
  ),
  targets::tar_target(
    name = table_spatial_sensitivity_provenance,
    command = tibble::tibble(
      analysis = "spatiotemporal_control",
      git_commit = system2(
        command = "git",
        args = c("rev-parse", "HEAD"),
        stdout = TRUE
      ),
      git_worktree_dirty = length(
        system2(
          command = "git",
          args = c("status", "--short"),
          stdout = TRUE
        )
      ) > 0L,
      seed = spatial_sensitivity_config[["seed"]],
      permutations = spatial_sensitivity_config[["permutations"]],
      thinning_repetitions =
        spatial_sensitivity_config[["thinning_repetitions"]],
      thinning_distances_km = stringr::str_c(
        spatial_sensitivity_config[["thinning_distances_km"]],
        collapse = ";"
      ),
      alpha = spatial_sensitivity_config[["alpha"]],
      min_unique_locations =
        spatial_sensitivity_config[["min_unique_locations"]],
      min_residual_df =
        spatial_sensitivity_config[["min_residual_df"]],
      min_unique_ages =
        spatial_sensitivity_config[["min_unique_ages"]],
      min_temporal_residual_df =
        spatial_sensitivity_config[["min_temporal_residual_df"]],
      temporal_distances_years = stringr::str_c(
        spatial_sensitivity_config[["temporal_distances_years"]],
        collapse = ";"
      ),
      adespatial_version = as.character(
        utils::packageVersion("adespatial")
      ),
      vegan_version = as.character(utils::packageVersion("vegan")),
      geosphere_version = as.character(
        utils::packageVersion("geosphere")
      ),
      metadata_md5 = unname(tools::md5sum(data_meta_path)),
      properties_filtered_md5 = unname(
        tools::md5sum(data_properties_filtered_path)
      ),
      properties_temporal_md5 = unname(
        tools::md5sum(data_properties_path)
      ),
      predictors_filtered_md5 = unname(
        tools::md5sum(data_predictors_filtered_path)
      ),
      predictors_temporal_md5 = unname(
        tools::md5sum(data_predictors_path)
      )
    )
  )
)
