#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis I
#
#
#                   O. Mottl, V. Felde
#                         2023
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


#----------------------------------------------------------#
# 1. Calculate spd density curves -----
#----------------------------------------------------------#

# - distances to calculate spd density curves
 spd_distance_vec <- 
  c(150, 250, 500) %>%
    rlang::set_names()

# - get polygons for each dataset_id
data_polygons <-
    get_polygons(
    data_source = data_meta,
    distance_buffer = 10 # 10° away from site
  )

# - a path for c14 data
data_c14_path <- 
    paste0(
    data_storage_path,
    "HOPE_Hypothesis1/Data/c14/data_rc_2022-11-29.rds"
  )
# - load c14 data
 data_c14 <- get_file_from_path(data_c14_path)

# - subset C14 data for each dataset_id and calculate distance to it
 data_c14_subset <-
   subset_c14_data(
    data_source_c14 = data_c14,
    data_source_polygons = data_polygons,
    data_source_meta = data_meta
  )

# - estimate spd for each distance
data_spd <- 
  get_spd(
    data_source_c14 = data_c14_subset,
    data_source_dist_vec = spd_distance_vec,
    age_from = min_age,
    age_to = max_age,
    age_timestep = timestep,
    min_n_dates = 50
  )


#### save here to data folder



