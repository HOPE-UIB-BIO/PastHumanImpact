#----------------------------------------------------------#
# Run SPD convergence with external human-impact proxies
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "human_proxy_convergence",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path =
      "sensitivity_analyses/human_proxy_convergence"
  ),
  visualise = FALSE
)
