#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#
#                    Filter meta data
#
#
#                  O. Mottl, V.A. Felde
#                         2024
#
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# 0.0. Setup -----
#--------------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

#---------------------------------------------------------------#
# 1. Filter meta data -----
#---------------------------------------------------------------#

# get path to the data assembly
file_assembly_path <-
  paste0(
    data_storage_path,
    "Data/assembly/data_assembly_V2-2022-05-23.rds"
  )
# - load data assembly from path
data_assembly <-
  get_data(file_assembly_path)

# - filter pollen data
data_assembly_filtered <-
  filter_all_data(data_assembly)


# - select meta data for filtered dataset_id
data_meta <-
  get_meta_data(
    data_assembly = data_assembly_filtered,
    variables = c(
      "dataset_id",
      "handle",
      "country",
      "long",
      "lat",
      "altitude",
      "depositionalenvironment",
      "region",
      "curve_name",
      "ecozone_koppen_5",
      "ecozone_koppen_15",
      "ecozone_koppen_30",
      "data_publicity",
      "doi"
    )
  )

#---------------------------------------------------------------#
# 2. Save processed meta data to data folder -----
#---------------------------------------------------------------#

readr::write_rds(
  x = data_meta,
  file = paste0(
    data_storage_path,
    "Data/assembly/data_meta-2024-01-30.rds"
  )
)
