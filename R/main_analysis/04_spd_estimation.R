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
    "R/project/00_Config_file.R"
  )
)


#----------------------------------------------------------#
# 1. Run individual scripts -----
#----------------------------------------------------------#

source(
  here::here(
    "R/spd_calculation/01_calculate_spd.R"
  )
)

source(
  here::here(
    "R/spd_calculation/02_calculate_spd_site_process.R"
  )
)

source(
  here::here(
    "R/spd_calculation/03_combine_spd.R"
  )
)

source(
  here::here(
    "R/spd_calculation/04_summary_spd_events.R"
  )
)
