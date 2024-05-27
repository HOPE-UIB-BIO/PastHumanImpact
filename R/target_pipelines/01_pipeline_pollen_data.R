#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                   Prepare pollen data
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

#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

# the targets list:
list(
  # 1. Pollen data prepartion -----
  # get path to the data assembly
  targets::tar_target(
    name = file_assembly_path,
    command = paste0(
      data_storage_path,
      "Data/assembly/data_assembly_V2-2022-05-23.rds"
    ),
    format = "file"
  ),
  # - load data assembly from path
  targets::tar_target(
    name = data_assembly,
    command = get_data(file_assembly_path)
  ),
  # - filter pollen data
  targets::tar_target(
    name = data_assembly_filtered,
    command = filter_all_data(data_assembly)
  ),
  # 2. Get pollen data and relevant variables for PAP estimation -----
  targets::tar_target(
    name = data_pollen,
    command = get_pollen_data(
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
