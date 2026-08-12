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
      min_residual_df = 10L
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
    command = get_file_from_path(data_meta_path)
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
    command = get_file_from_path(data_properties_filtered_path)
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
    command = get_file_from_path(data_properties_path)
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
    command = get_file_from_path(data_predictors_filtered_path)
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
    command = get_file_from_path(data_predictors_path)
  ),
  targets::tar_target(
    name = data_hvar_filtered,
    command = get_data_combined(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  targets::tar_target(
    name = data_properties_temporal,
    command = get_data_filtered(
      data_source = data_properties,
      data_meta = data_meta,
      age_from = 0,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  targets::tar_target(
    name = data_predictors_temporal,
    command = get_data_filtered(
      data_source = data_predictors,
      data_meta = data_meta,
      age_from = 0,
      age_to = 8500,
      remove_private = TRUE
    )
  ),
  targets::tar_target(
    name = data_hvar_temporal,
    command = get_data_combined(
      data_source_properties = data_properties_temporal,
      data_source_predictors = data_predictors_temporal
    )
  ),
  targets::tar_target(
    name = data_hvar_timebins_spd,
    command = get_data_timebin(
      data_source = data_hvar_temporal,
      data_meta = data_meta
    ) |>
      dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500))
  ),
  targets::tar_target(
    name = output_spatial_spd_issue325,
    command = run_hvarpart(
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
    name = data_spatial_importance_issue325,
    command = get_hvarpart_importance(
      data_source = output_spatial_spd_issue325 |>
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
        dplyr::mutate(analysis = "spatial_spd_issue325"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    )
  ),
  targets::tar_target(
    name = data_figure2_spatial_records,
    command = prepare_spatial_importance_records(
      data_importance = data_spatial_importance_issue325,
      data_meta = data_meta
    )
  ),
  targets::tar_target(
    name = output_figure2_spatial_filter,
    command = analyse_spatial_importance(
      data_records = data_figure2_spatial_records,
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
    name = data_figure2_thinning_ledger,
    command = run_spatial_thinning(
      data_source = data_figure2_spatial_records,
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
    name = table_figure2_spatial_sensitivity,
    command = summarise_spatial_importance_sensitivity(
      data_records = data_figure2_spatial_records,
      data_thinning = data_figure2_thinning_ledger
    )
  ),
  targets::tar_target(
    name = table_figure2_spatial_estimates,
    command = output_figure2_spatial_filter[["estimates"]]
  ),
  targets::tar_target(
    name = table_figure2_moran_diagnostics,
    command = output_figure2_spatial_filter[["moran_diagnostics"]]
  ),
  targets::tar_target(
    name = table_figure2_dbmem_diagnostics,
    command = output_figure2_spatial_filter[["dbmem"]][["diagnostics"]]
  ),
  targets::tar_target(
    name = table_figure2_dbmem_selection,
    command = tibble::tibble(
      status = output_figure2_spatial_filter[["selection"]][["status"]],
      n_complete =
        output_figure2_spatial_filter[["selection"]][["n_complete"]],
      n_candidates =
        output_figure2_spatial_filter[["selection"]][["n_candidates"]],
      global_p_value =
        output_figure2_spatial_filter[["selection"]][["global_p_value"]],
      full_adjusted_r_squared = purrr::pluck(
        output_figure2_spatial_filter,
        "selection",
        "full_adjusted_r_squared"
      ),
      selected_names = stringr::str_c(
        output_figure2_spatial_filter[["selection"]][["selected_names"]],
        collapse = ";"
      )
    )
  ),
  targets::tar_target(
    name = table_figure2_robustness,
    command = classify_spatial_robustness(
      data_sensitivity = table_figure2_spatial_sensitivity,
      data_spatial_estimates = table_figure2_spatial_estimates
    ) |>
      dplyr::left_join(
        table_figure2_moran_diagnostics |>
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
    command = split(
      data_hvar_timebins_spd,
      seq_len(nrow(data_hvar_timebins_spd))
    ),
    iteration = "list"
  ),
  targets::tar_target(
    name = output_temporal_spatial_group,
    command = list(
      region = data_hvar_timebin_groups[["region"]][1],
      age = data_hvar_timebin_groups[["age"]][1],
      result = analyse_spatial_hvarpart_group(
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
    name = table_temporal_partial_fractions,
    command = output_temporal_spatial_group |>
      purrr::map_dfr(
        .f = ~ .x[["result"]][["partial_fractions"]] |>
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
          data_baseline <-
            .x[["result"]][["baseline_hvarpart"]][["summary_table"]] |>
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
          dplyr::bind_rows(data_baseline, data_spatial) |>
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
        baseline_balance =
          .data[["human_climate_human"]] -
          .data[["human_climate_climate"]],
        spatial_balance = dplyr::coalesce(
          .data[["human_climate_space_human"]] -
            .data[["human_climate_space_climate"]],
          .data[["baseline_balance"]]
        ),
        baseline_ranking = dplyr::case_when(
          .data[["baseline_balance"]] > 0 ~ "human",
          .data[["baseline_balance"]] < 0 ~ "climate",
          .default = "tie"
        ),
        spatial_ranking = dplyr::case_when(
          .data[["spatial_balance"]] > 0 ~ "human",
          .data[["spatial_balance"]] < 0 ~ "climate",
          .default = "tie"
        ),
        ranking_changed =
          .data[["baseline_ranking"]] != .data[["spatial_ranking"]]
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
    name = table_spatial_sensitivity_provenance,
    command = tibble::tibble(
      analysis = "issue_325_spatial_dependence",
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
