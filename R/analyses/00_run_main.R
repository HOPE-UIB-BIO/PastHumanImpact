#----------------------------------------------------------#
# Run canonical project analyses in dependency order
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

pipeline_contracts <-
  readr::read_csv(
    here::here(
      "R",
      "analyses",
      "00_profiles",
      "pipeline_contracts.csv"
    ),
    show_col_types = FALSE
  )

validate_pipeline_contract_registry(pipeline_contracts)

vec_main_runners <-
  c(
    "01_data_preparation/00_run.R",
    "02_h1_spatiotemporal_hvarpart/00_run.R",
    "03_temporal_models/00_run.R",
    "04_h2_multidimensional_shifts/00_run.R",
    "05_visualisations/00_run.R",
    "06_reporting/00_run.R"
  )

purrr::walk(
  .x = vec_main_runners,
  .f = ~ source(here::here("R", "analyses", .x))
)
