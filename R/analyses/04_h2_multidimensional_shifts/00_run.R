#----------------------------------------------------------#
# Run H2 multidimensional-shift pipeline
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

run_target_pipeline(
  script = here::here(
    "R",
    "analyses",
    "04_h2_multidimensional_shifts",
    "pipeline.R"
  ),
  store = resolve_pipeline_store_path(
    data_storage_path = data_storage_path,
    store_relative_path = "analyses_h2/multidimensional_shifts"
  ),
  visualise = FALSE
)
