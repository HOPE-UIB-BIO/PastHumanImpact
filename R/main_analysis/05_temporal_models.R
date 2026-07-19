#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#

#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# Running the models is very computationally expensive. Therefore, we have to
#   decide whether we want to rerun the models or not.
rerun <- FALSE


#----------------------------------------------------------#
# 1. Run individual scripts -----
#----------------------------------------------------------#

if (
  isTRUE(rerun)
) {
  source(
    here::here(
      "R/temporal_models/01_prepare_data.R"
    )
  )

  source(
    here::here(
      "R/temporal_models/02_prepare_data_and_priors.R"
    )
  )

  source(
    here::here(
      "R/temporal_models/03_prepare_model_config_table.R"
    )
  )

  source(
    here::here(
      "R/temporal_models/04_run_models.R"
    )
  )

  source(
    here::here(
      "R/temporal_models/05_evaluate_models.R"
    )
  )

  source(
    here::here(
      "R/temporal_models/06_predict_general_trends.R"
    )
  )
}
