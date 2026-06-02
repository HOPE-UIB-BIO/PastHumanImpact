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
    "R/supplementary_analyses/Collinearity/05_pipeline_h1_reviewer_collinearity.R"
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
      "R/supplementary_analyses/Collinearity/05_pipeline_h1_reviewer_collinearity.R"
    ),
    store = file.path(
      data_storage_path,
      "Targets_data",
      "analyses_h1_reviewer_collinearity"
    )
  )
}

#----------------------------------------------------------#
# 2. Build reviewer collinearity outputs -----
#----------------------------------------------------------#

source(
  here::here(
    "R/supplementary_analyses/Collinearity/02_run_collinearity_outputs.R"
  )
)

# end of script ----
