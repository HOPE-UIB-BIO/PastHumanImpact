#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                        Meta data
#
#
#                   O. Mottl, V.A. Felde
#                         2023
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
# 1. Load and filter meta data -----
#----------------------------------------------------------#

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

#----------------------------------------------------------#
# 2. Split meta data into different tibbles -----
#----------------------------------------------------------#


# - meta data regions
regions <- 
  data_meta %>%
  dplyr::select(
    dataset_id,
    region
  ) 

# - meta data climate zones
climate_zones <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    climatezone
  ) 

# - meta data locations
locations <- 
  data_meta %>%
  dplyr::select(
    dataset_id,
    long,
    lat
  )

# meta data altitude
altitude <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    altitude
  )

# - meta data age ranges
age_range <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    age_min,
    age_max
  )

# - meta data publication status
publication_status <-
  data_meta %>%
  dplyr::select(
    dataset_id,
    data_publicity
  )
