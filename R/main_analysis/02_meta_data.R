#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                        Meta data
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
    "R/project/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load processed meta data -----
#----------------------------------------------------------#

# - path to meta data
file_meta_data_path <-
  paste0(
    data_storage_path,
    "Data/assembly/data_meta-2024-01-30.rds"
  )
# - load processed meta data
data_meta <-
  get_file_from_path(file_meta_data_path)


#----------------------------------------------------------#
# 2. Split meta data  -----
#----------------------------------------------------------#


# - meta data regions ----
regions <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    region
  )

# - meta data climate zones ----
climate_zones <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    climatezone
  )

# - meta data locations ----
locations <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    long,
    lat
  )

# meta data altitude -----
altitude <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    altitude
  )

# - meta data mean age ranges -----
age_range <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    age_min,
    age_max
  )

# - meta data publication status ----
publication_status <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    data_publicity
  )
