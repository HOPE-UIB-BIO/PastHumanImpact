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

# - load data assembly from path
data_assembly <-
  RUtilpol::get_latest_file(
    file_name = "data_assembly",
    dir = paste0(
      data_storage_path,
      "Assembly/"
    )
  )

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

RUtilpol::save_latest_file(
  file_name = "data_meta",
  dir = paste0(
    data_storage_path,
    "Meta/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)
