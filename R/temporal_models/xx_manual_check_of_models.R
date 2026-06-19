#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                   manual check of models
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)


#----------------------------------------------------------#
# 1. Evaluate a model -----
#----------------------------------------------------------#

sel_region <- "Europe"
sel_climatezone <- "Temperate_Dry_Summer"
sel_variable <- "prec_summer"
sel_model_id <- paste("predictor_temporal", sel_variable, sep = "__")

mod <-
  RUtilpol::get_latest_file(
    file_name = sel_model_id,
    dir = paste0(
      data_storage_path,
      "Temporal_models/Mods"
    ),
    verbose = TRUE
  )

summary(mod)

plot(mod)

brms::loo(mod)

brms::rhat(mod)

brms::pp_check(mod, ndraws = 1000, type = "stat")

brms::pp_check(mod, ndraws = 1000, type = "dens_overlay")
