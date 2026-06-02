#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#            Run reviewer collinearity workflow
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

# Reviewer-only runner for issue #329 collinearity sensitivity.

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)

visualise_pipeline <- TRUE

#----------------------------------------------------------#
# 1. Run reviewer H1 sensitivity pipeline -----
#----------------------------------------------------------#

targets::tar_make(
  script = here::here(
    "R/target_pipelines/05_pipeline_hvar_spatial_temporal_reviewer_collinearity.R"
  ),
  store = file.path(
    data_storage_path,
    "Targets_data",
    "analyses_h1_reviewer_collinearity"
  )
)

if (
  isTRUE(visualise_pipeline)
) {
  targets::tar_visnetwork(
    targets_only = TRUE,
    script = here::here(
      "R/target_pipelines/05_pipeline_hvar_spatial_temporal_reviewer_collinearity.R"
    ),
    store = file.path(
      data_storage_path,
      "Targets_data",
      "analyses_h1_reviewer_collinearity"
    )
  )
}

#----------------------------------------------------------#
# 2. Build reviewer collinearity figure -----
#----------------------------------------------------------#

source(
  here::here(
    "R/visualisations/extended_data_analysis/EDA_5_pap_collinearity.R"
  )
)

# end of script ----
