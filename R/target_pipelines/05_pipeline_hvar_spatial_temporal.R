#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                 Run analyses for Hypothesis I
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#



#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)


#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

list(
  # load data_properties filtered range 2000-8500 ----
  targets::tar_target(
    name = data_properties_filtered_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_paps/objects/data_properties_filtered"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_properties_filtered,
    command = get_file_from_path(data_properties_filtered_path)
  ),

  # load data_properties unfiltered range for temporal analysis ----
  targets::tar_target(
    name = data_properties_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_paps/objects/data_properties"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_properties,
    command = get_file_from_path(data_properties_path)
  ),
  # load data_predictors ----
  targets::tar_target(
    name = data_predictor_filtered_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_predictors/objects/data_predictors_filtered"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_predictors_filtered,
    command = get_file_from_path(data_predictor_filtered_path)
  ),
  # load data_predictors unfiltered range for temporal analysis ----
  targets::tar_target(
    name = data_predictor_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_predictors/objects/data_predictors"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_predictors,
    command = get_file_from_path(data_predictor_path)
  ),
  # - filter data predictors for temporal from 0-8500 ----
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
  # - filter data predictors for temporal from 0-8500 ----
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
  # - combine properties and predictors for hvar temporal ----
  targets::tar_target(
    name = data_hvar_temporal,
    command = get_data_combined(
      data_source_properties = data_properties_temporal,
      data_source_predictors = data_predictors_temporal
    )
  ),
  # - combine properties and predictors for hvar spatial ----
  targets::tar_target(
    name = data_hvar_filtered,
    command = get_data_combined(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  # - get data for timebins; input range age from 0-8500 ----
  targets::tar_target(
    name = data_hvar_timebins,
    command = get_data_timebin(
      data_source = data_hvar_temporal,
      data_meta = data_meta
    )
  ),
  # - Hierarchical variation partitioning: ----
  # - run spatial (within core) analysis with spd age from 2000 ----
  targets::tar_target(
    name = output_spatial_spd,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
      response_dist = NULL,
      data_response_dist = NULL,
      response_vars = c(
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
      ),
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        )
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999
    )
  ),
  # - run spatial (within core) analysis with events ----
  targets::tar_target(
    name = output_spatial_events,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
      response_dist = NULL,
      data_response_dist = NULL,
      response_vars = c(
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
      ),
      predictor_vars = list(
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
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        )
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999
    )
  ),
  # - run temporal analysis with spd ----
  targets::tar_target(
    name = output_temporal_spd,
    command = run_hvarpart(
      data_source = data_hvar_timebins,
      response_dist = NULL,
      data_response_dist = NULL,
      response_vars = c(
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
      ),
      predictor_vars = list(
        human = c("spd"),
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        )
      ),
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE,
      permutations = 999
    )
  ),
  # - run temporal analysis with events ----
  targets::tar_target(
    name = output_temporal_events,
    command = run_hvarpart(
      data_source = data_hvar_timebins,
      response_dist = NULL,
      data_response_dist = NULL,
      response_vars = c(
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
      ),
      predictor_vars = list(
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
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        )
      ),
      run_all_predictors = FALSE,
      time_series = FALSE,
      get_significance = FALSE,
      permutations = 999
    )
  ),
  # - preserve raw HVarPart components and diagnostics ----
  targets::tar_target(
    name = data_hvarpart_spatial_spd_importance,
    command = get_hvarpart_importance(
      data_source = output_spatial_spd |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              dataset_id,
              region,
              climatezone
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_spd"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    )
  ),
  targets::tar_target(
    name = data_hvarpart_spatial_events_importance,
    command = get_hvarpart_importance(
      data_source = output_spatial_events |>
        dplyr::left_join(
          data_meta |>
            dplyr::select(
              dataset_id,
              region,
              climatezone
            ),
          by = "dataset_id"
        ) |>
        dplyr::mutate(analysis = "spatial_events"),
      id_cols = c(
        "analysis",
        "dataset_id",
        "region",
        "climatezone"
      )
    )
  ),
  targets::tar_target(
    name = data_hvarpart_temporal_spd_importance,
    command = get_hvarpart_importance(
      data_source = output_temporal_spd |>
        dplyr::mutate(analysis = "temporal_spd"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    )
  ),
  targets::tar_target(
    name = data_hvarpart_temporal_events_importance,
    command = get_hvarpart_importance(
      data_source = output_temporal_events |>
        dplyr::mutate(analysis = "temporal_events"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    )
  ),
  targets::tar_target(
    name = data_hvarpart_h1_importance,
    command = dplyr::bind_rows(
      data_hvarpart_spatial_spd_importance,
      data_hvarpart_spatial_events_importance,
      data_hvarpart_temporal_spd_importance,
      data_hvarpart_temporal_events_importance
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h1_audit_overall,
    command = summarise_hvarpart_audit(
      data_importance = data_hvarpart_h1_importance,
      group_vars = "analysis"
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h1_profiles_overall,
    command = compare_hvarpart_importance_profiles(
      data_importance = data_hvarpart_h1_importance,
      group_vars = "analysis"
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h1_audit_spatial,
    command = data_hvarpart_h1_importance |>
      dplyr::filter(
        .data[["analysis"]] %in% c(
          "spatial_spd",
          "spatial_events"
        )
      ) |>
      summarise_hvarpart_audit(
        group_vars = c(
          "analysis",
          "region",
          "climatezone"
        )
      )
  ),
  targets::tar_target(
    name = table_hvarpart_h1_profiles_spatial,
    command = data_hvarpart_h1_importance |>
      dplyr::filter(
        .data[["analysis"]] %in% c(
          "spatial_spd",
          "spatial_events"
        )
      ) |>
      compare_hvarpart_importance_profiles(
        group_vars = c(
          "analysis",
          "region",
          "climatezone"
        )
      )
  ),
  targets::tar_target(
    name = table_hvarpart_h1_audit_temporal,
    command = data_hvarpart_h1_importance |>
      dplyr::filter(
        .data[["analysis"]] %in% c(
          "temporal_spd",
          "temporal_events"
        )
      ) |>
      summarise_hvarpart_audit(
        group_vars = c(
          "analysis",
          "region",
          "age"
        )
      )
  ),
  targets::tar_target(
    name = table_hvarpart_h1_profiles_temporal,
    command = data_hvarpart_h1_importance |>
      dplyr::filter(
        .data[["analysis"]] %in% c(
          "temporal_spd",
          "temporal_events"
        )
      ) |>
      compare_hvarpart_importance_profiles(
        group_vars = c(
          "analysis",
          "region",
          "age"
        )
      )
  )
)
