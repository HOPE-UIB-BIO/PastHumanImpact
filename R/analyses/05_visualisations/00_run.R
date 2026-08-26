#----------------------------------------------------------#
# Run canonical project visualisations
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

vec_visualisation_scripts <-
  c(
    "data/dataset_locations.R",
    "data/human_impact_coverage.R",
    "data/pollen_dataset_coverage.R",
    "h1/dataset_trends/all_dataset_trends.R",
    "h1/dataset_trends/example_datasets.R",
    "h1/spatial/human_climate_balance.R",
    "h1/spatial/spatial_dependence.R",
    "h1/temporal/event_trends.R",
    "h1/temporal/human_climate_space_composition.R",
    "h1/temporal/pap_trends.R",
    "h1/temporal/predictor_trends.R",
    "h2/predictor_interrelationships.R"
  )

purrr::walk(
  .x = vec_visualisation_scripts,
  .f = ~ source(
    here::here("R", "analyses", "05_visualisations", .x)
  )
)
