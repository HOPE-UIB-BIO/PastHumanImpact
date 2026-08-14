#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#          H1 predictor collinearity sensitivity
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
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)


#----------------------------------------------------------#
# 1. Upstream contracts -----
#----------------------------------------------------------#

store_paps <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/paps"
  )

store_predictors <-
  resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/predictors"
  )

runner_data_preparation <-
  "R/analyses/01_data_preparation/00_run.R"

#----------------------------------------------------------#
# 2. Targets -----
#----------------------------------------------------------#

list(
  # load data_properties filtered range 2000-8500 ----
  targets::tar_target(
    name = fingerprint_paps,
    command = compute_target_store_fingerprint(
      store = store_paps,
      target_names = "data_properties_filtered",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = data_properties_filtered,
    command = {
      fingerprint_paps

      load_target_store_value(
        store = store_paps,
        target_name = "data_properties_filtered",
        runner = runner_data_preparation
      )
    }
  ),

  # load data_predictors ----
  targets::tar_target(
    name = fingerprint_predictors,
    command = compute_target_store_fingerprint(
      store = store_predictors,
      target_names = "data_predictors_filtered",
      runner = runner_data_preparation
    ),
    cue = targets::tar_cue(mode = "always")
  ),
  targets::tar_target(
    name = data_predictors_filtered,
    command = {
      fingerprint_predictors

      load_target_store_value(
        store = store_predictors,
        target_name = "data_predictors_filtered",
        runner = runner_data_preparation
      )
    }
  ),
  # - combine properties and predictors for hvar spatial ----
  targets::tar_target(
    name = data_hvar_filtered,
    command = prepare_combined_data(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  # - flatten nested hvar data for collinearity diagnostics ----
  targets::tar_target(
    name = data_hvar_filtered_flat,
    command = data_hvar_filtered %>%
      tidyr::unnest(
        cols = data_merge
      )
  ),
  # - join spatial metadata for diagnostic grouping ----
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
    command = diagnose_pap_collinearity(
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
    command = select_reduced_pap_predictors(
      data_collinearity = output_pap_collinearity_spatial,
      min_selected_fraction = 0.5
    )
  ),
  # - Hierarchical variation partitioning: baseline spatial SPD ----
  targets::tar_target(
    name = output_spatial_spd,
    command = fit_hvarpart_models(
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
    command = fit_hvarpart_models(
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
