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
  # File path for filtered H1 pollen properties.
  targets::tar_target(
    name = data_properties_filtered_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_paps/objects/data_properties_filtered"
    ),
    format = "file"
  ),
  # Filtered pollen properties for spatial H1 models.
  targets::tar_target(
    name = data_properties_filtered,
    command = resolve_file_path(data_properties_filtered_path)
  ),

  # load data_properties unfiltered range for temporal analysis ----
  # File path for unfiltered H1 pollen properties.
  targets::tar_target(
    name = data_properties_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_paps/objects/data_properties"
    ),
    format = "file"
  ),
  # Unfiltered pollen properties for temporal H1 models.
  targets::tar_target(
    name = data_properties,
    command = resolve_file_path(data_properties_path)
  ),
  # load data_predictors ----
  # File path for filtered H1 predictor data.
  targets::tar_target(
    name = data_predictor_filtered_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_predictors/objects/data_predictors_filtered"
    ),
    format = "file"
  ),
  # Filtered predictors for spatial H1 models.
  targets::tar_target(
    name = data_predictors_filtered,
    command = resolve_file_path(data_predictor_filtered_path)
  ),
  # load data_predictors unfiltered range for temporal analysis ----
  # File path for unfiltered H1 predictor data.
  targets::tar_target(
    name = data_predictor_path,
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_predictors/objects/data_predictors"
    ),
    format = "file"
  ),
  # Unfiltered predictors for temporal H1 models.
  targets::tar_target(
    name = data_predictors,
    command = resolve_file_path(data_predictor_path)
  ),
  # - filter data predictors for temporal from 0-8500 ----
  # Public temporal predictors restricted to 0-8.5 ka.
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
  # - filter data predictors for temporal from 0-8500 ----
  # Public temporal pollen properties restricted to 0-8.5 ka.
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
  # - combine properties and predictors for hvar temporal ----
  # Combined temporal properties and predictors for H1.
  targets::tar_target(
    name = data_hvar_temporal,
    command = prepare_combined_data(
      data_source_properties = data_properties_temporal,
      data_source_predictors = data_predictors_temporal
    )
  ),
  # - combine properties and predictors for hvar spatial ----
  # Combined filtered properties and predictors for H1.
  targets::tar_target(
    name = data_hvar_filtered,
    command = prepare_combined_data(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  # - get data for timebins; input range age from 0-8500 ----
  # Temporal H1 data aggregated into analysis time bins.
  targets::tar_target(
    name = data_hvar_timebins,
    command = prepare_hvarpart_timebin_data(
      data_source = data_hvar_temporal,
      data_meta = data_meta
    )
  ),
  # Temporal H1 SPD bins restricted to the valid 2-8.5 ka range.
  targets::tar_target(
    name = data_hvar_timebins_spd,
    command = data_hvar_timebins |>
      dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500))
  ),
  # - Hierarchical variation partitioning: ----
  # - run spatial (within core) analysis with spd age from 2000 ----
  # Spatial H1 partitioning with SPD as human predictor.
  targets::tar_target(
    name = output_spatial_spd,
    command = fit_hvarpart_models(
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
  # Spatial H1 partitioning with event predictors.
  targets::tar_target(
    name = output_spatial_events,
    command = fit_hvarpart_models(
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
  # Temporal H1 SPD partitioning restricted to 2-8.5 ka.
  targets::tar_target(
    name = output_temporal_spd,
    command = fit_hvarpart_models(
      data_source = data_hvar_timebins_spd,
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
  # Temporal H1 partitioning with event predictors.
  targets::tar_target(
    name = output_temporal_events,
    command = fit_hvarpart_models(
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
  # Raw signed importance from spatial SPD models.
  targets::tar_target(
    name = data_hvarpart_spatial_spd_importance,
    command = compute_hvarpart_importance(
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
  # Raw signed importance from spatial event models.
  targets::tar_target(
    name = data_hvarpart_spatial_events_importance,
    command = compute_hvarpart_importance(
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
  # Raw signed importance from temporal SPD models.
  targets::tar_target(
    name = data_hvarpart_temporal_spd_importance,
    command = compute_hvarpart_importance(
      data_source = output_temporal_spd |>
        dplyr::filter(dplyr::between(.data[["age"]], 2000, 8500)) |>
        dplyr::mutate(analysis = "temporal_spd"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    )
  ),
  # Raw signed importance from temporal event models.
  targets::tar_target(
    name = data_hvarpart_temporal_events_importance,
    command = compute_hvarpart_importance(
      data_source = output_temporal_events |>
        dplyr::mutate(analysis = "temporal_events"),
      id_cols = c(
        "analysis",
        "region",
        "age"
      )
    )
  ),
  # Combined raw signed importance for all H1 analyses.
  targets::tar_target(
    name = data_hvarpart_h1_importance,
    command = dplyr::bind_rows(
      data_hvarpart_spatial_spd_importance,
      data_hvarpart_spatial_events_importance,
      data_hvarpart_temporal_spd_importance,
      data_hvarpart_temporal_events_importance
    )
  ),
  # Overall model eligibility audit for each H1 analysis.
  targets::tar_target(
    name = table_hvarpart_h1_audit_overall,
    command = summarise_hvarpart_audit(
      data_importance = data_hvarpart_h1_importance,
      group_vars = "analysis"
    )
  ),
  # Overall H1 comparison of importance profiles.
  targets::tar_target(
    name = table_hvarpart_h1_profiles_overall,
    command = diagnose_hvarpart_importance_profiles(
      data_importance = data_hvarpart_h1_importance,
      group_vars = "analysis"
    )
  ),
  # Spatial H1 eligibility audit by region and climate zone.
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
  # Spatial H1 profile comparison by region and climate zone.
  targets::tar_target(
    name = table_hvarpart_h1_profiles_spatial,
    command = data_hvarpart_h1_importance |>
      dplyr::filter(
        .data[["analysis"]] %in% c(
          "spatial_spd",
          "spatial_events"
        )
      ) |>
      diagnose_hvarpart_importance_profiles(
        group_vars = c(
          "analysis",
          "region",
          "climatezone"
        )
      )
  ),
  # Temporal H1 eligibility audit by region and age.
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
  # Temporal H1 profile comparison by region and age.
  targets::tar_target(
    name = table_hvarpart_h1_profiles_temporal,
    command = data_hvarpart_h1_importance |>
      dplyr::filter(
        .data[["analysis"]] %in% c(
          "temporal_spd",
          "temporal_events"
        )
      ) |>
      diagnose_hvarpart_importance_profiles(
        group_vars = c(
          "analysis",
          "region",
          "age"
      )
    )
  ),
  # Signed and bounded model decompositions for all H1 analyses.
  targets::tar_target(
    name = data_hvarpart_h1_decomposition,
    command = compute_hvarpart_variance_decomposition(
      data_importance = data_hvarpart_h1_importance,
      id_cols = c(
        "analysis",
        "model_id",
        "dataset_id",
        "region",
        "climatezone",
        "age"
      )
    )
  ),
  # Overall signed and bounded H1 variance summaries.
  targets::tar_target(
    name = table_hvarpart_h1_variance_overall,
    command = summarise_hvarpart_variance_decomposition(
      data_decomposition = data_hvarpart_h1_decomposition,
      group_vars = "analysis"
    )
  ),
  # Spatial H1 variance summaries by continent and climate.
  targets::tar_target(
    name = table_hvarpart_h1_variance_spatial,
    command = data_hvarpart_h1_decomposition |>
      dplyr::filter(
        .data[["analysis"]] %in% c("spatial_spd", "spatial_events")
      ) |>
      summarise_hvarpart_variance_decomposition(
        group_vars = c("analysis", "region", "climatezone")
      )
  ),
  # Temporal H1 variance summaries by continent and age.
  targets::tar_target(
    name = table_hvarpart_h1_variance_temporal,
    command = data_hvarpart_h1_decomposition |>
      dplyr::filter(
        .data[["analysis"]] %in% c("temporal_spd", "temporal_events")
      ) |>
      summarise_hvarpart_variance_decomposition(
        group_vars = c("analysis", "region", "age")
      )
  ),
  # Model values for spatial SPD fit-importance correlations.
  targets::tar_target(
    name = data_hvarpart_spatial_spd_correlation_values,
    command = compute_hvarpart_correlation_values(
      data_importance = data_hvarpart_spatial_spd_importance,
      id_cols = c(
        "analysis",
        "model_id",
        "dataset_id",
        "region",
        "climatezone"
      )
    )
  ),
  # Overall signed fit-importance correlation.
  targets::tar_target(
    name = table_hvarpart_spatial_spd_correlation_overall,
    command = summarise_hvarpart_correlations(
      data_values = data_hvarpart_spatial_spd_correlation_values,
      importance_column = "human_importance_signed"
    )
  ),
  # Continent-climate signed fit-importance correlations.
  targets::tar_target(
    name = table_hvarpart_spatial_spd_correlation_grid,
    command = summarise_hvarpart_correlations(
      data_values = data_hvarpart_spatial_spd_correlation_values,
      group_vars = c("region", "climatezone"),
      importance_column = "human_importance_signed"
    )
  )
)
