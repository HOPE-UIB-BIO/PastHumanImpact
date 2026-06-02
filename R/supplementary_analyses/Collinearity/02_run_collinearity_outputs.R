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
    "R/supplementary_analyses/Collinearity/EDA_5a_pap_collinearity_correlation.R"
  )
)

source(
  here::here(
    "R/supplementary_analyses/Collinearity/EDA_5b_pap_collinearity_hvar_difference.R"
  )
)

source(
  here::here(
    "R/supplementary_analyses/Collinearity/EDA_5c_h1_spatial_reduced_simple.R"
  )
)
