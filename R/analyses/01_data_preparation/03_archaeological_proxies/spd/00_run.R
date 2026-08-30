#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     SPD calculation
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#

# Run the pipeline-managed SPD calculation graph.

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

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "01_data_preparation",
    "03_archaeological_proxies",
    "spd",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "data_preparation/spd"
  ),
  visualise = FALSE
)
