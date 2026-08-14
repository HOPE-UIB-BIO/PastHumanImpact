#----------------------------------------------------------#
# Run optional project diagnostics
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

vec_diagnostic_scripts <-
  c(
    "pollen_filtering/rarefaction_diagnostics.R",
    "pollen_filtering/rarefaction_visualisations.R",
    "spatiotemporal_dependence/00_run.R"
  )

purrr::walk(
  .x = vec_diagnostic_scripts,
  .f = ~ source(
    here::here("R", "analyses", "90_diagnostics", .x)
  )
)
