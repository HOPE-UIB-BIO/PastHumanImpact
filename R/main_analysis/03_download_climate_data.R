#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#
#                    Download climate data
#
#
#                   O. Mottl, V.A. Felde
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
    "R/project/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)

#---------------------------------------------------------------#
# 1. Download CHELSA palaeoclimate data -----
#---------------------------------------------------------------#

# - a path to time reference table (from code)
time_ref_path <-
  paste0(
    data_storage_path,
    "Data/climate/time_reference_table.rds"
  )
# - load table
time_ref_table <- get_file_from_path(time_ref_path)

# - download CHELSA data
data_climate_chelsa <-
  get_climate_data(
    variables_selected = c("bio", "tasmin"),
    bio_var_selected = c(1, 6, 12, 15, 18, 19),
    time_var_selected = c(20:-200),
    month_var_selected = c(1:12),
    xy = data_meta
  )

# - get climate variables
data_climate <-
  get_climate_indices(
    data_source = data_climate_chelsa,
    time_ref = time_ref_table
  )


#---------------------------------------------------------------#
# 2. Save palaeoclimate data to data folder -----
#---------------------------------------------------------------#

readr::write_rds(
  x = data_climate,
  file = paste0(
    data_storage_path,
    "Data/climate/data_climate-2024-01-23.rds"
  )
)
