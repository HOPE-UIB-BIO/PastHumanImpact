#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Temporal model inputs
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the temporal model inputs target graph.
# Run with:
#   R/analyses/03_temporal_models/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # Why: Track temporal model data as a file target so file changes invalidate
  #   downstream results.
  targets::tar_target(
    name = "file_temporal_model_data",
    command = file.path(
      path_temporal_models,
      RUtilpol::get_latest_file_name(
        file_name = "general_temporal_model_data",
        dir = path_temporal_models
      )
    ),
    format = "file"
  ),
  # Why: Prepare temporal model so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_temporal_model",
    command = resolve_file_path(file_temporal_model_data)
  ),
  # Why: Track temporal model specifications as a file target so file changes
  #   invalidate downstream results.
  targets::tar_target(
    name = "file_temporal_model_specifications",
    command = file.path(
      path_temporal_models,
      RUtilpol::get_latest_file_name(
        file_name = "general_temporal_model_specs",
        dir = path_temporal_models
      )
    ),
    format = "file"
  ),
  # Why: Prepare temporal model specifications so downstream targets share one
  #   canonical dataset.
  targets::tar_target(
    name = "data_temporal_model_specifications",
    command = resolve_file_path(file_temporal_model_specifications)
  ),
  # Why: Define temporal model input hash once so downstream targets use one
  #   reproducible value.
  targets::tar_target(
    name = "temporal_model_input_hash",
    command = unname(tools::md5sum(file_temporal_model_data))
  )
)
