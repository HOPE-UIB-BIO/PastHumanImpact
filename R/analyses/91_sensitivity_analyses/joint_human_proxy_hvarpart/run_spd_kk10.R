#----------------------------------------------------------#
# Run SPD plus KK10 H1 HVarPart using the shared human-proxy graph
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

# The store selects the profile declared for it in the script_profile column
#   of R/analyses/00_profiles/pipeline_contracts.csv.
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
    store_relative_path = "sensitivity_analyses/spd_kk10_hvarpart"
  ),
  visualise = FALSE
)
