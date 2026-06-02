#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#         Reviewer collinearity pipeline entrypoint
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/target_pipelines/05_pipeline_hvar_spatial_temporal_reviewer_collinearity.R"
  ),
  local = TRUE
)$value
