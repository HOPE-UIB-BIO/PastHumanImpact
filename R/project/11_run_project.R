#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis I
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

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

targets::tar_config_set(
  script = here::here("R/hypothesis_1/h1_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_h1"
  ),
  project = "project_h1"
)

Sys.setenv(TAR_PROJECT = "project_h1")


#----------------------------------------------------------#
# 1. run target pipeline -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here("R/hypothesis_1/h1_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_h1"
  )
)