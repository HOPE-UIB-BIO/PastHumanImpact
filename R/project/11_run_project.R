#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Run project
#
#
#                   O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)


#----------------------------------------------------------#
# 1. run target pipeline pollen data -----
#----------------------------------------------------------#

targets::tar_make(
     script = here::here("R/project/03_pipeline_pollen_data.R"),
     store = paste0(
         data_storage_path,
         "_targets_data"
     )
 )

# check pipeline pollen data
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/03_pipeline_pollen_data.R"),
  store = paste0(
    data_storage_path,
    "_targets_data"
  )
)


#----------------------------------------------------------#
# 2. run target pipeline events data -----
#----------------------------------------------------------#
targets::tar_make(
     script = here::here("R/project/04_pipeline_events.R"),
     store = paste0(
         data_storage_path,
         "_targets_data"
     )
 )

# check pipeline events data
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/04_pipeline_events.R"),
  store = paste0(
    data_storage_path,
    "_targets_data"
  )
)

#----------------------------------------------------------#
# 3. run target pipeline predictors -----
#----------------------------------------------------------#
targets::tar_make(
  script = here::here("R/project/05_pipeline_predictors.R"),
  store = paste0(
    data_storage_path,
    "_targets_data"
  )
)

# check pipeline predictors
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/05_pipeline_predictors.R"),
  store = paste0(
    data_storage_path,
    "_targets_data"
  )
)

#----------------------------------------------------------#
# 4. run target pipeline hvar spatial & temporal (h1) -----
#----------------------------------------------------------#
targets::tar_make(
     script = here::here("R/project/06_pipeline_hvar_spatial_temporal.R"),
     store = paste0(
         data_storage_path,
         "_targets_h1"
     )
 )

# check hvar spatial & temporal (h1)
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/06_pipeline_hvar_spatial_temporal.R"),
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)

#----------------------------------------------------------#
# 5. run target pipeline multidimensional shifts (h2) -----
#----------------------------------------------------------#
targets::tar_make(
     script = here::here("R/project/09_pipeline_multidimensional_shifts.R"),
     store = paste0(
         data_storage_path,
         "_targets_h2"
     )
 )