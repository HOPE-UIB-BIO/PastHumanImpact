#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Pollen data preparation
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#
# Defines the pollen data preparation target graph.
# Run with:
#   R/analyses/01_data_preparation/00_run.R
# Sourcing this script only declares targets; it does not execute them.

#----------------------------------------------------------#
# 0. Configure pipeline -----
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
    "R/analyses/01_data_preparation/01_metadata/02_metadata.R"
  )
)

#----------------------------------------------------------#
# 1. Define targets -----
#----------------------------------------------------------#

list(
  # 1. Pollen data prepartion -----
  # get path to the data assembly
  # Why: Track assembly path as a file target so file changes invalidate
  #   downstream results.
  targets::tar_target(
    name = "file_assembly_path",
    command = RUtilpol::get_latest_file_name(
      file_name = "data_assembly",
      dir = paste0(
        data_storage_path,
        "Assembly/"
      )
    ),
    format = "file"
  ),
  # - load data assembly from path
  # Why: Prepare assembly so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_assembly",
    command = resolve_file_path(file_assembly_path)
  ),
  # - filter pollen data
  # Why: Prepare assembly filtered so downstream targets share one canonical
  #   dataset.
  targets::tar_target(
    name = "data_assembly_filtered",
    command = filter_all_data(data_assembly)
  ),
  # 2. Get pollen data and relevant variables for PAP estimation -----
  # Why: Prepare pollen so downstream targets share one canonical dataset.
  targets::tar_target(
    name = "data_pollen",
    command = prepare_pollen_data(
      data_assembly = data_assembly_filtered,
      variables = c(
        "dataset_id",
        "counts_harmonised",
        "levels",
        "age_uncertainty",
        "end_of_interest_period",
        "pollen_percentage"
      )
    )
  )
) # end of targets
