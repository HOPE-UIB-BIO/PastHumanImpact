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


#----------------------------------------------------------#
# 1. Run individual scripts -----
#----------------------------------------------------------#

source(
  here::here(
    "R/spd_calculation/01_calculate_spd_250.R"
  )
)

source(
  here::here(
    "R/spd_calculation/02_calculate_spd_500.R"
  )
)

source(
  here::here(
    "R/spd_calculation/03_combine_spd.R"
  )
)