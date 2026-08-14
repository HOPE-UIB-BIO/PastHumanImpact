#----------------------------------------------------------#
# Run optional project sensitivity analyses
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

source(
  here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "predictor_collinearity",
    "00_run.R"
  )
)

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "spatiotemporal_robustness",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = paste(
      "sensitivity_analyses",
      "spatiotemporal_robustness",
      sep = "/"
    )
  ),
  visualise = FALSE
)
