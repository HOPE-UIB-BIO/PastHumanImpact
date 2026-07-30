#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
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

# Load configuration
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

mod_config_file <- RUtilpol::get_latest_file(
  file_name = "general_model_config_table",
  dir = paste0(
    data_storage_path,
    "Temporal_models/"
  )
)

#----------------------------------------------------------#
# 2. Target pipeline -----
#----------------------------------------------------------#

# the targets list:
list(
  # - path to data for multidimensional shifts ----
  targets::tar_target(
    name = data_m2_path,
    description = "File path for filtered multidimensional-shift data.",
    command = paste0(
      data_storage_path,
      "Targets_data/pipeline_paps/objects/data_m2_filtered"
    ),
    format = "file"
  ),
  # - load data for multidimensional shifts
  targets::tar_target(
    name = data_m2_filtered,
    description = "Filtered multidimensional-shift data for H2.",
    command = get_file_from_path(data_m2_path)
  ),
  # # - get the model configuration file
  # targets::tar_target(
  #   name = mod_config_file,
  #   command = RUtilpol::get_latest_file(
  #     file_name = "general_model_config_table",
  #     dir = paste0(
  #       data_storage_path,
  #       "Temporal_models/"
  #     )
  #   )
  # ),
  # - load all models
  targets::tar_target(
    name = mod_predicted_merged,
    description = "Merged temporal-model predictions used as H2 predictors.",
    command = get_all_predicted_general_trends(
      data_source = mod_config_file
    )
  ),
  # - merge datasets for hvar analyses of multidimensional shifts
  targets::tar_target(
    name = data_for_hvar_h2,
    description = "Combined shifts and predictors for H2 partitioning.",
    command = get_data_for_h2_hvar(
      data_m2 = data_m2_filtered,
      data_predictors = mod_predicted_merged
    )
  ),
  # - run hierarchical variation partitioning
  targets::tar_target(
    name = output_hvar_h2_spd,
    description = "H2 partitioning with SPD and climate predictors.",
    command = run_hvarpart(
      data_source = data_for_hvar_h2,
      response_vars = NULL,
      response_dist = NULL,
      data_response_dist = "m2",
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
      get_significance = FALSE
    )
  ),
  # - preserve raw HVarPart components and diagnostics ----
  targets::tar_target(
    name = data_hvarpart_h2_importance,
    description = "Raw signed importance from H2 models.",
    command = get_hvarpart_importance(
      data_source = output_hvar_h2_spd |>
        dplyr::mutate(analysis = "h2_spd"),
      id_cols = c(
        "analysis",
        "region",
        "climatezone"
      )
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h2_audit_overall,
    description = "Overall model eligibility audit for H2.",
    command = summarise_hvarpart_audit(
      data_importance = data_hvarpart_h2_importance,
      group_vars = "analysis"
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h2_profiles_overall,
    description = "Overall H2 comparison of importance profiles.",
    command = compare_hvarpart_importance_profiles(
      data_importance = data_hvarpart_h2_importance,
      group_vars = "analysis"
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h2_audit_strata,
    description = "H2 eligibility audit by region and climate zone.",
    command = summarise_hvarpart_audit(
      data_importance = data_hvarpart_h2_importance,
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    )
  ),
  targets::tar_target(
    name = table_hvarpart_h2_profiles_strata,
    description = "H2 profile comparison by region and climate zone.",
    command = compare_hvarpart_importance_profiles(
      data_importance = data_hvarpart_h2_importance,
      group_vars = c(
        "analysis",
        "region",
        "climatezone"
      )
    )
  )
)
