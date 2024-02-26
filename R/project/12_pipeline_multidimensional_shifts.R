#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
#
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
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
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_paps/objects/data_m2_filtered"
    ),
    format = "file"
  ),
  # - load data for multidimensional shifts
  targets::tar_target(
    name = data_m2_filtered,
    command = get_file_from_path(data_m2_path)
  ),
  # - get the model configuration file
  targets::tar_target(
    name = mod_config_file,
    command = RUtilpol::get_latest_file_name(
      file_name = "predictor_models_config_table",
      dir = paste0(
        data_storage_path,
        "Data/Predictor_models/"
      )
    ),
    format = "file"
  ),
  # - load all models
  targets::tar_target(
    name = mod_predicted_merged,
    command = get_all_predicted_general_trends(
      data_source = mod_config_file
    )
  ),
  # - merge datasets for hvar analyses of multidimensional shifts
  targets::tar_target(
    name = data_for_hvar_h2,
    command = get_data_for_h2_hvar(
      data_m2 = data_m2_filtered,
      data_predictors = mod_predicted_merged
    )
  ),
  # // TODO: debug and run
  # - run hierarchical variation partitioning
  targets::tar_target(
    name = output_hvar_h2_spd,
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
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999
    )
  )
)
