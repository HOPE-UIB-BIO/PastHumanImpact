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
    "R/00_Config_file.R"
  )
)

#----------------------------------------------------------#
# 1. Load processed meta data -----
#----------------------------------------------------------#

# - load processed meta data
data_meta <-
  RUtilpol::get_latest_file(
    file_name = "data_meta",
    dir = paste0(
      data_storage_path,
      "Assembly/"
    )
  )


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
