#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#
#                    Download climate data
#
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

rewrite <- FALSE


#---------------------------------------------------------------#
# 1. Download CHELSA palaeoclimate data -----
#---------------------------------------------------------------#

if (
  rewrite ||
    isTRUE(
      is.na(
        RUtilpol::get_latest_file_name(
          file_name = "data_climate",
          dir = paste0(
            data_storage_path,
            "Climate/"
          )
        )
      )
    )
) {
  # - load table
  time_ref_table <-
    get_file_from_path(
      paste0(
        data_storage_path,
        "Climate/time_reference_table.rds"
      )
    )

  # - download CHELSA data
  data_climate_chelsa <-
    get_climate_data(
      variables_selected = c("bio", "tasmin"),
      bio_var_selected = c(1, 6, 12, 15, 18, 19),
      time_var_selected = c(20:-200),
      month_var_selected = c(1:12),
      xy = data_meta
    )

  # - get climate variables
  data_climate <-
    get_climate_indices(
      data_source = data_climate_chelsa,
      time_ref = time_ref_table
    )

  # Save palaeoclimate data to data folder
  RUtilpol::save_latest_file(
    object_to_save = data_climate,
    dir = paste0(
      data_storage_path,
      "Climate/"
    ),
    prefered_format = "rds",
    use_sha = FALSE
  )
}
