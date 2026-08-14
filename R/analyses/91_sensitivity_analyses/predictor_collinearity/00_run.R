#----------------------------------------------------------#
# Run H1 predictor-collinearity sensitivity
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "predictor_collinearity",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "sensitivity_analyses/predictor_collinearity"
  ),
  visualise = FALSE
)

source(
  here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "predictor_collinearity",
    "run_outputs.R"
  )
)
