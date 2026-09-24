#----------------------------------------------------------#
# Run joint SPD, KK10, and HYDE HVarPart sensitivity
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "joint_human_proxy_hvarpart",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path =
      "sensitivity_analyses/joint_human_proxy_hvarpart"
  ),
  visualise = FALSE
)
