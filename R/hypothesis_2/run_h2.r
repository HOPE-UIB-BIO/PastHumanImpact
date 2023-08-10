#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis II
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
  script = here::here("R/hypothesis_2/h2_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  ),
  project = "project_h2"
)

Sys.setenv(TAR_PROJECT = "project_h2")


#----------------------------------------------------------#
# 2. run target pipeline -----
#----------------------------------------------------------#

# check the expected targets
targets::tar_manifest(
  script = here::here("R/hypothesis_2/h2_target_pipeline.R")
) %>%
  View()

# run the target pipeline
targets::tar_make(
  script = here::here("R/hypothesis_2/h2_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  )
)

#----------------------------------------------------------#
# 2.  check workflow -----
#----------------------------------------------------------#

targets::tar_visnetwork(
  script = here::here("R/hypothesis_2/h2_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_h2"
  ),
  targets_only = TRUE
)
