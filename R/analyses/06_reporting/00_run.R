#----------------------------------------------------------#
# Run stable reporting contracts
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "06_reporting",
    "evidence_manifest",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "reporting/evidence_manifest"
  ),
  visualise = FALSE
)
