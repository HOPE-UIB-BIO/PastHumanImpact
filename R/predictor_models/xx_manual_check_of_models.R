#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
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
    "R/project/00_Config_file.R"
  )
)


#----------------------------------------------------------#
# 1. Evaluate a model -----
#----------------------------------------------------------#

sel_region <- "Europe"
sel_climatezone <- "Temperate_Dry_Summer"
sel_variable <- "prec_summer"

mod <-
  RUtilpol::get_latest_file(
    file_name = paste(
      sel_variable,
      sel_region,
      sel_climatezone,
      sep = "__"
    ),
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/Mods"
    ),
    verbose = TRUE
  )

summary(mod)

plot(mod)

brms::loo(mod)

brms::rhat(mod)

brms::pp_check(mod, ndraws = 1000, type = "stat")

brms::pp_check(mod, ndraws = 1000, type = "dens_overlay")
