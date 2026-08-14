#----------------------------------------------------------#
# Temporal-model persistent inputs
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

list(
  targets::tar_target(
    name = file_temporal_model_data,
    command = file.path(
      path_temporal_models,
      RUtilpol::get_latest_file_name(
        file_name = "general_temporal_model_data",
        dir = path_temporal_models
      )
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_temporal_model,
    command = resolve_file_path(file_temporal_model_data)
  ),
  targets::tar_target(
    name = file_temporal_model_specifications,
    command = file.path(
      path_temporal_models,
      RUtilpol::get_latest_file_name(
        file_name = "general_temporal_model_specs",
        dir = path_temporal_models
      )
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_temporal_model_specifications,
    command = resolve_file_path(file_temporal_model_specifications)
  ),
  targets::tar_target(
    name = temporal_model_input_hash,
    command = unname(tools::md5sum(file_temporal_model_data))
  )
)
