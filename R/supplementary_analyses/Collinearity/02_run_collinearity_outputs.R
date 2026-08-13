#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#         Reviewer collinearity output orchestrator
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/supplementary_analyses/Collinearity/pap_collinearity_correlation.R"
  )
)

source(
  here::here(
    "R/supplementary_analyses/Collinearity/pap_collinearity_hvarpart_influence.R"
  )
)

source(
  here::here(
    "R/supplementary_analyses/Collinearity/human_climate_balance_reduced_predictors.R"
  )
)
