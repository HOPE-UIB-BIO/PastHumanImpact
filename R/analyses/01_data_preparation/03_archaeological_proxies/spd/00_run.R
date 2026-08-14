#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     SPD calculation
#
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#

# Run all scripts for the SPD calculation

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

rewrite <- FALSE


#----------------------------------------------------------#
# 1. Run individual scripts -----
#----------------------------------------------------------#

if (
  isTRUE(rewrite) ||
    isTRUE(
      is.na(
        RUtilpol::get_latest_file_name(
          file_name = "data_spd_250",
          dir = paste0(
            data_storage_path,
            "SPD/"
          )
        )
      )
    )
) {
  source(
    here::here(
      paste0(
        "R/analyses/01_data_preparation/",
        "03_archaeological_proxies/spd/01_calculate_spd_250.R"
      )
    )
  )
}

if (
  isTRUE(rewrite) ||
    isTRUE(
      is.na(
        RUtilpol::get_latest_file_name(
          file_name = "data_spd_500",
          dir = paste0(
            data_storage_path,
            "SPD/"
          )
        )
      )
    )
) {
  source(
    here::here(
      paste0(
        "R/analyses/01_data_preparation/",
        "03_archaeological_proxies/spd/02_calculate_spd_500.R"
      )
    )
  )
}


source(
  here::here(
    paste0(
      "R/analyses/01_data_preparation/",
      "03_archaeological_proxies/spd/03_combine_spd.R"
    )
  )
)
