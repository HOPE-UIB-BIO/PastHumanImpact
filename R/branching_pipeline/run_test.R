#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  testing target functionality
#
#
#                   O. Mottl, V. Felde
#                         2023
#
#----------------------------------------------------------#

# This is a sxript to test run `test_target_pipeline.R`

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

targets::tar_config_set(
  script = here::here("R//test_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_test"
  ),
  project = "project_test"
)

Sys.setenv(TAR_PROJECT = "project_test")


# You can use `targets::tar_manifest()` to check the expected targets
targets::tar_manifest(
  script = here::here("R/branching_pipeline/test_target_pipeline.R")
) %>%
  View()

# View the status of the pipe
targets::tar_visnetwork(
  script = here::here("R/branching_pipeline/test_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_test"
  ),
  targets_only = TRUE,
  label = "branches"
)

# Run the targest
targets::tar_make(
  script = here::here("R/branching_pipeline/test_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)

# Check the original data
targets::tar_read(
  name = "data_dummy_merge",
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)

# Check one of the branch
targets::tar_read(
  name = "data_subset_Asia",
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)

# Check the combined data
targets::tar_read(
  name = "data_combined_static",
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)

targets::tar_read(
  name = "data_combined_dynamic",
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)

targets::tar_read(
  name = "data_compare",
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)

targets::tar_prune(
  script = here::here("R/branching_pipeline/test_target_pipeline.R"),
  store = paste0(
    data_storage_path,
    "_targets_test"
  )
)
