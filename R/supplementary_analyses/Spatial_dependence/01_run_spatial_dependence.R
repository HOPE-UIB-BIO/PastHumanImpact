#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#          Run reviewer spatial-dependence workflow
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)

path_script <-
  here::here(
    "R/supplementary_analyses/Spatial_dependence",
    "05_pipeline_h1_spatial_dependence.R"
  )
path_store <-
  file.path(
    data_storage_path,
    "Targets_data",
    "analyses_h1_reviewer_spatiotemporal_control"
  )

targets::tar_make(
  script = path_script,
  store = path_store
)

source(
  here::here(
    "R/supplementary_analyses/Spatial_dependence/02_export_spatial_dependence.R"
  )
)

# end of script ----
