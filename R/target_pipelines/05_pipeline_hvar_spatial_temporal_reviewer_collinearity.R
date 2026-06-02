#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#      Run reviewer sensitivity for Hypothesis I (H1)
#
#                   O. Mottl, V.A. Felde
#                         2026
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

  # load data_predictors ----
  targets::tar_target(
    name = data_predictor_filtered_path,
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
    command = get_file_from_path(data_predictor_filtered_path)
  ),
  # - combine properties and predictors for hvar spatial ----
  targets::tar_target(
    name = data_hvar_filtered,
    command = get_data_combined(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  # - flatten nested hvar data for reviewer collinearity diagnostics ----
  targets::tar_target(
    name = data_hvar_filtered_flat,
    command = data_hvar_filtered %>%
      tidyr::unnest(
        cols = data_merge
      )
  ),
  # - join spatial metadata for reviewer grouping ----
  targets::tar_target(
    name = data_hvar_filtered_with_meta,
    command = dplyr::left_join(
      data_hvar_filtered_flat,
      data_meta %>%
        dplyr::select(
          dataset_id,
          region,
          climatezone
        ),
      by = "dataset_id"
    )
  ),
  # - baseline H1 PAP response variables ----
  targets::tar_target(
    name = pap_response_vars_h1_baseline,
    command = c(
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
  ),
  # - PAP collinearity summary for H1 spatial data ----
  targets::tar_target(
    name = output_pap_collinearity_spatial,
    command = get_pap_collinearity(
      data_source = data_hvar_filtered_with_meta,
      pap_vars = pap_response_vars_h1_baseline,
      group_var = c("region", "climatezone"),
      preference_order = pap_response_vars_h1_baseline,
      max_cor = 0.8,
      max_vif = 5,
      min_rows = 30,
      quiet = TRUE
    )
  ),
  # - reduced PAP response set for collinearity sensitivity ----
  targets::tar_target(
    name = pap_response_vars_h1_reduced_collinear_v1,
    command = get_pap_reduced_vars(
      data_collinearity = output_pap_collinearity_spatial,
      min_selected_fraction = 0.5
    )
  ),
  # - Hierarchical variation partitioning: baseline spatial SPD ----
  targets::tar_target(
    name = output_spatial_spd,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
      response_dist = NULL,
      data_response_dist = NULL,
      response_vars = pap_response_vars_h1_baseline,
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
      permutations = 999,
      fail_on_error = FALSE
    )
  ),
  # - reduced PAP sensitivity: spatial SPD ----
  targets::tar_target(
    name = output_spatial_spd_reduced_collinear_v1,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
      response_dist = NULL,
      data_response_dist = NULL,
      response_vars = pap_response_vars_h1_reduced_collinear_v1,
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
      permutations = 999,
      fail_on_error = FALSE
    )
  )
)
