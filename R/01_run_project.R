#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Run project
#
#
#                   O. Mottl, V. Felde
#                         2024
#
#----------------------------------------------------------#

# Main script, which will run the entire project.
#   This will produce all the figures presented in the paper.

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

visualise_pipeline <- FALSE


#----------------------------------------------------------#
# 1. Prepare meta data -----
#----------------------------------------------------------#

source(
  here::here(
    "R/main_analysis/01_filter_meta_data.R"
  )
)


#----------------------------------------------------------#
# 2. Dowload all climate data -----
#----------------------------------------------------------#

# !!! Warning: This will take a while to run !!!
source(
  here::here(
    "R/main_analysis/03_download_climate_data.R"
  )
)


#----------------------------------------------------------#
# 3. Prepare all SPD values -----
#----------------------------------------------------------#

# !!! Warning: This will take a while to run !!!
source(
  here::here(
    "R/main_analysis/04_spd_estimation.R"
  )
)


#----------------------------------------------------------#
# 4. run target pipeline pollen data -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/01_pipeline_pollen_data.R"
  ),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_pollen_data"
  )
)

# check pipeline pollen data
if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/01_pipeline_pollen_data.R"
    ),
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_pollen_data"
    )
  )
}


#----------------------------------------------------------#
# 5. run target pipeline pollen assemblage properties -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/02_pipeline_paps.R"
  ),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_paps"
  )
)

# check pipeline PAPs
if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/02_pipeline_paps.R"
    ),
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_paps"
    )
  )
}


#----------------------------------------------------------#
# 6. run target pipeline events data -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/03_pipeline_events.R"
  ),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_events"
  )
)

# check pipeline events data
if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/03_pipeline_events.R"
    ),
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_events"
    )
  )
}


#----------------------------------------------------------#
# 7. run target pipeline predictors -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/04_pipeline_predictors.R"
  ),
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_predictors"
  )
)

# check pipeline predictors
if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/04_pipeline_predictors.R"
    ),
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors"
    )
  )
}


#----------------------------------------------------------#
# 8. run target pipeline hvar spatial & temporal (H1) -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/05_pipeline_hvar_spatial_temporal.R"
  ),
  store = paste0(
    data_storage_path,
    "_targets_data/analyses_h1"
  )
)

# check hvar spatial & temporal
if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/05_pipeline_hvar_spatial_temporal.R"
    ),
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h1"
    )
  )
}

#----------------------------------------------------------#
# 9. run all predictore gerenral trends (H2) -----
#----------------------------------------------------------#

# !!! Warning: This will take a while to run !!!
#   see the script for details about settings

source(
  here::here(
    "R/main_analysis/05_prediction_models"
  )
)


#----------------------------------------------------------#
# 10. run target pipeline multidimensional shifts (H2) -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/06_pipeline_multidimensional_shifts.R"
  ),
  store = paste0(
    data_storage_path,
    "_targets_data/analyses_h2"
  )
)

if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/06_pipeline_multidimensional_shifts.R"
    ),
    store = paste0(
      data_storage_path,
      "_targets_data/analyses_h2"
    )
  )
}

#----------------------------------------------------------#
# 11. run scripts for figures ----
#----------------------------------------------------------#

# combined figure of h1 1 ----
source(
  here::here(
    "R/visualisations/01_Figure_2_H1_spatial.R"
  )
)

# combined figure of h1 2 ----
source(
  here::here(
    "R/visualisations/02_Figure_3_H1_temporal.R"
  )
)

# combined figure of h2 ----
source(
  here::here(
    "R/visualisations/03_Figure_4_H2_interrelationships.R"
  )
)

# end of script ----
