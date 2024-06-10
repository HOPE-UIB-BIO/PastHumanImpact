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

# Will skip if data is already downloaded
#  and stored in the data_storage_path
# If you want to redownload the data, set the
#  variable 'rewrite' to TRUE in the script
# !!! Warning: This will take a while to run !!!
source(
  here::here(
    "R/main_analysis/03_download_climate_data.R"
  )
)


#----------------------------------------------------------#
# 3. Prepare all SPD values -----
#----------------------------------------------------------#

# Will skip if data is already downloaded
#  and stored in the data_storage_path
# If you want to redownload the data, set the
#  variable 'rewrite' to TRUE in the script
# !!! Warning: This will take a while to run !!!
source(
  here::here(
    "R/main_analysis/04_spd_estimation.R"
  )
)


#----------------------------------------------------------#
# 4. Run target pipeline pollen data -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/01_pipeline_pollen_data.R"
  ),
  store = paste0(
    data_storage_path,
    "Targets_data/pipeline_pollen_data"
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
      "Targets_data/pipeline_pollen_data"
    )
  )
}


#----------------------------------------------------------#
# 5. Run target pipeline pollen assemblage properties -----
#----------------------------------------------------------#

# !!! Warning: This will take a while to run !!!
targets::tar_make(
  script = here::here(
    "R/target_pipelines/02_pipeline_paps.R"
  ),
  store = paste0(
    data_storage_path,
    "Targets_data/pipeline_paps"
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
      "Targets_data/pipeline_paps"
    )
  )
}


#----------------------------------------------------------#
# 6. Run target pipeline events data -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/03_pipeline_events.R"
  ),
  store = paste0(
    data_storage_path,
    "Targets_data/pipeline_events"
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
      "Targets_data/pipeline_events"
    )
  )
}


#----------------------------------------------------------#
# 7. Run target pipeline predictors -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/04_pipeline_predictors.R"
  ),
  store = paste0(
    data_storage_path,
    "Targets_data/pipeline_predictors"
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
      "Targets_data/pipeline_predictors"
    )
  )
}


#----------------------------------------------------------#
# 8. Run target pipeline hvar spatial & temporal (H1) -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/05_pipeline_hvar_spatial_temporal.R"
  ),
  store = paste0(
    data_storage_path,
    "Targets_data/analyses_h1"
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
      "Targets_data/analyses_h1"
    )
  )
}

#----------------------------------------------------------#
# 9. Run all predictore gerenral trends (H2) -----
#----------------------------------------------------------#

# # Running the models is very computationally expensive.
#   Therefore, it is curently set to to skip.
# If you prefer to manually rerun all model, you need to:
#   1.set it set the variable 'rerun' to TRUE in the
#     `R/main_analysis/05_prediction_models.r` script a
#   2. manualy flagg the models to rerun in the
#     `Predictor_models/predictor_models_config_table.csv`.
#     Specifically, column `need_to_run` should be set to TRUE.
#   !!! Warning: This will take a while to run !!!
source(
  here::here(
    "R/main_analysis/05_prediction_models"
  )
)


#----------------------------------------------------------#
# 10. Run target pipeline multidimensional shifts (H2) -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/06_pipeline_multidimensional_shifts.R"
  ),
  store = paste0(
    data_storage_path,
    "Targets_data/analyses_h2"
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
      "Targets_data/analyses_h2"
    )
  )
}

#----------------------------------------------------------#
# 11. Run scripts for figures ----
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
