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
     script = here::here("R/project/05_pipeline_pollen_data.R"),
     store = paste0(
         data_storage_path,
         "_targets_data/pipeline_pollen_data"
     )
 )

# check pipeline pollen data
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/05_pipeline_pollen_data.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_pollen_data"
  )
)

#----------------------------------------------------------#
# 2. run target pipeline pollen assemblage properties -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here("R/project/06_pipeline_paps.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_paps"
  )
)

# check pipeline pollen data
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/06_pipeline_paps.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_paps"
  )
)

#----------------------------------------------------------#
# 3. run target pipeline events data -----
#----------------------------------------------------------#
targets::tar_make(
     script = here::here("R/project/07_pipeline_events.R"),
     store = paste0(
         data_storage_path,
         "_targets_data/pipeline_events"
     )
 )

# check pipeline events data
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/07_pipeline_events.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_events"
  )
)

#----------------------------------------------------------#
# 4. run target pipeline predictors -----
#----------------------------------------------------------#
targets::tar_make(
  script = here::here("R/project/08_pipeline_predictors.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_predictors"
  )
)

# check pipeline predictors
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/08_pipeline_predictors.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_predictors"
  )
)

#----------------------------------------------------------#
# 5. run target pipeline hvar spatial & temporal (h1) -----
#----------------------------------------------------------#
targets::tar_make(
     script = here::here("R/project/09_pipeline_hvar_spatial_temporal.R"),
     store = paste0(
         data_storage_path,
         "_targets_data/analyses_h1"
     )
 )

# check hvar spatial & temporal (h1)
targets::tar_visnetwork(
  targets_only = TRUE,
  script = here::here("R/project/09_pipeline_hvar_spatial_temporal.R"),
  store = paste0(
    data_storage_path,
    "_targets_data/analyses_h1"
  )
)

#----------------------------------------------------------#
# 6. run target pipeline multidimensional shifts (h2) -----
#----------------------------------------------------------#
targets::tar_make(
     script = here::here("R/project/09_pipeline_multidimensional_shifts.R"),
     store = paste0(
         data_storage_path,
         "_targets_data/analyses_h2"
     )
 )