#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                    Prepare all data
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

source(
  here::here(
    "R/00_Config_file.R"
  )
)

source(
  here::here(
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_predictors <-
  targets::tar_read(
    name = "data_predictors",
    store = paste0(
      data_storage_path,
      "Targets_data/pipeline_predictors"
    )
  )

data_properties <-
  targets::tar_read(
    name = "data_properties",
    store = paste0(
      data_storage_path,
      "Targets_data/pipeline_paps"
    )
  )


#----------------------------------------------------------#
# 2. Prepare predictors, events, and PAPs -----
#----------------------------------------------------------#

list_predictor_model_data <-
  prepare_predictor_model_data(
    data_predictors = data_predictors,
    data_meta = data_meta,
    data_regions = regions,
    data_climatezones = climate_zones,
    age_from = 0,
    age_to = 8500,
    min_records = min_n_records_per_climate_zone,
    remove_private = TRUE
  )

data_pap_model <-
  prepare_pap_model_data(
    data_properties = data_properties,
    data_meta = data_meta,
    pap_vars = c(
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
    age_from = 0,
    age_to = 8500,
    min_records = min_n_records_per_climate_zone
  ) %>%
  dplyr::mutate(
    analysis = "pap_temporal"
  ) %>%
  dplyr::select(
    analysis,
    dplyr::everything()
  )

data_general_model <-
  dplyr::bind_rows(
    list_predictor_model_data[["data_model"]],
    data_pap_model
  )


#----------------------------------------------------------#
# 3. Save -----
#----------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_general_model,
  file_name = "general_temporal_model_data",
  dir = paste0(
    data_storage_path,
    "Temporal_models/"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = data_pap_model,
  file_name = "pap_temporal_model_data",
  dir = paste0(
    data_storage_path,
    "Temporal_models/"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)

RUtilpol::save_latest_file(
  object_to_save = list_predictor_model_data[["data_constant"]],
  file_name = "temporal_model_data_constant",
  dir = paste0(
    data_storage_path,
    "Temporal_models/"
  ),
  prefered_format = "rds",
  use_sha = TRUE
)
