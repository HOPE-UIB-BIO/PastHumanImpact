#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#        Extended data analysis - PAP collinearity
#             Orchestrator for split scripts
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/visualisations/extended_data_analysis/EDA_5a_pap_collinearity_correlation.R"
  )
)

source(
  here::here(
    "R/visualisations/extended_data_analysis/EDA_5b_pap_collinearity_hvar_difference.R"
  )
)

source(
  here::here(
    "R/visualisations/extended_data_analysis/EDA_5c_h1_spatial_reduced_simple.R"
  )
)
