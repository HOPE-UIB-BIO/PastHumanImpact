#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                Prepare model config table
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

overweite_table <- FALSE

#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_to_fit",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )


#----------------------------------------------------------#
# 1. Make default table -----
#----------------------------------------------------------#

model_config_table <-
  data_to_fit %>%
  dplyr::distinct(region, climatezone, variable, n_records) %>%
  dplyr::mutate(
    total_iterations = 3200,
    min_iterations_per_chain = 100,
    last_run_date = NA_character_,
    last_run_start_time = NA_character_,
    last_run_end_time = NA_character_,
    last_run_time = NA_character_,
    last_run_rhat_test_pass = FALSE,
    last_run_rhat_test_value = NA_real_,
    last_run_loo_test_pass = FALSE,
    last_run_loo_test_value = NA_real_,
    need_to_run = TRUE,
    need_to_be_evaluated = FALSE,
    last_evaluation_date = NA_character_
  ) %>%
  dplyr::arrange(
    region, variable, climatezone
  )


#----------------------------------------------------------#
# 2. Save table -----
#----------------------------------------------------------#

if (
  isTRUE(overweite_table)
) {
  RUtilpol::save_latest_file(
    object_to_save = model_config_table,
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    ),
    prefered_format = "csv"
  )
}
