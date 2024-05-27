#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#
#                     Calculate SPD
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# 0.0. Setup -----
#--------------------------------------------------------------#

library(here)
library(furrr)

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

# - Load meta data
source(
  here::here(
    "R/project/02_meta_data.R"
  )
)


#---------------------------------------------------------------#
# 1. Load and prepare subsets of C14 dates -----
#---------------------------------------------------------------#

# - distances to calculate spd density curves
spd_distance_vec <-
  c(500) %>%
  rlang::set_names()

# - get polygons for each dataset_id
data_polygons <-
  get_polygons(
    data_source = data_meta,
    distance_buffer = 10 # 10° away from site
  )

# - path for c14 data
data_c14_path <-
  paste0(
    data_storage_path,
    "Data/c14/data_rc_2022-11-29.rds"
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

#---------------------------------------------------------------#
# 2. Calculate spd for each distance per locality -----
#---------------------------------------------------------------#

n_cores <-
  as.numeric(
    parallelly::availableCores()
  )

future::plan(
  future::multicore,
  workers = n_cores - 1
)

# split
data_c14_list <- data_c14_subset %>%
  split(.$dataset_id)

# calculate spd
data_spd <- furrr::future_map(
  .progress = TRUE,
  data_c14_list, ~ get_spd(
    data_source_c14 = .x,
    data_source_dist_vec = spd_distance_vec,
    sel_smooth_size = 100,
    min_n_dates = 50,
    age_from = min_age, # [config]
    age_to = max_age # [config]
  )
) %>%
  dplyr::bind_rows()



#---------------------------------------------------------------#
# 3. Save spd data to data folder ----
#---------------------------------------------------------------#


RUtilpol::save_latest_file(
  object_to_save = data_spd,
  file_name = "data_spd",
  dir = paste0(
    data_storage_path,
    "Data/spd/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)

#---------------------------------------------------------------#
