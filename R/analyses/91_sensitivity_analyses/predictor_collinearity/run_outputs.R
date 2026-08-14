#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#          Predictor collinearity output orchestrator
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#

library(here)

source(
  here::here(
    paste0(
      "R/analyses/91_sensitivity_analyses/predictor_collinearity/",
      "pap_collinearity_correlation.R"
    )
  )
)

source(
  here::here(
    paste0(
      "R/analyses/91_sensitivity_analyses/predictor_collinearity/",
      "pap_collinearity_hvarpart_influence.R"
    )
  )
)

source(
  here::here(
    paste0(
      "R/analyses/91_sensitivity_analyses/predictor_collinearity/",
      "human_climate_balance_reduced_predictors.R"
    )
  )
)
