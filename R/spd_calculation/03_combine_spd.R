#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#
#                    Combine data spd
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# 0.0. Setup -----
#--------------------------------------------------------------#

library(here)

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/main_analysis/02_meta_data.R"
  )
)

#--------------------------------------------------------------#
# 1.0. Load data spd 250 and 500 distance  -----
#--------------------------------------------------------------#
# - load data spd distance 250 ----
data_spd_250 <-
  RUtilpol::get_latest_file(
    file_name = "data_spd",
    dir = paste0(
      data_storage_path,
      "Data/spd/"
    )
  )

# - load data spd distance 250 ----
data_spd_500 <-
  RUtilpol::get_latest_file(
    file_name = "data_spd_500",
    dir = paste0(
      data_storage_path,
      "Data/spd/"
    )
  )

#--------------------------------------------------------------#
# 2.0. Replace data spd 500 with missing data  -----
#--------------------------------------------------------------#

data_spd_combine <-
  data_spd_250 %>%
  dplyr::filter(!dataset_id %in% data_spd_500$dataset_id) %>%
  mutate(spd = purrr::map(spd,
    .f = ~ .x %>%
      rename(value = `250`)
  )) %>%
  mutate(distance = 250) %>%
  dplyr::full_join(data_spd_500 %>%
    mutate(
      spd = purrr::map(spd,
        .f = ~ .x %>%
          rename(value = `500`)
      )
    ) %>%
    mutate(distance = 500))


#--------------------------------------------------------------#
# 3.0. Save  -----
#--------------------------------------------------------------#
# save data_spd_combine ----
RUtilpol::save_latest_file(
  object_to_save = data_spd_combine,
  file_name = "data_spd_combine",
  dir = paste0(
    data_storage_path,
    "Data/spd/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)

#---------------------------------------------------------------#
