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

sel_mod_config <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = file.path(
      data_storage_path,
      "Temporal_models"
    )
  ) %>%
  dplyr::filter(
    analysis == "predictor_temporal",
    region == sel_region,
    climatezone == sel_climatezone,
    variable == sel_variable
  )

assertthat::assert_that(
  nrow(sel_mod_config) == 1L,
  msg = "The selected values must identify exactly one model."
)

sel_model_id <-
  sel_mod_config[["model_id"]][1]

mod <-
  load_brms_model_file(
    model_dir = file.path(
      data_storage_path,
      "Temporal_models",
      "Mods"
    ),
    model_file_name = sel_mod_config[["model_file_name"]][1],
    model_id = sel_model_id
  )

summary(mod)

plot(mod)

brms::loo(mod)

brms::rhat(mod)

brms::pp_check(mod, ndraws = 1000, type = "stat")

brms::pp_check(mod, ndraws = 1000, type = "dens_overlay")
