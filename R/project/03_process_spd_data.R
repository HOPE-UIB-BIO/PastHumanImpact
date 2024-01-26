#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                   Pre-process spd data
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
    "R/project/00_Config_file.R"
  )
)
# Load meta data
source(
  here::here(
    "R/project/01_meta_data.R"
  )
)

#----------------------------------------------------------#
# 1. Load and prepare subsets of C14 dates -----
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

 # - calculate spd in parallell session (base windows)

cores <- parallel::detectCores() - 1  # leave one core 
cl <- parallel::makeCluster(cores)

data_c14_subset %>% 
  split(.$dataset_id) %>% 
  purrr::map(
    .x = .,
    .f = function(x) {
      parallel::clusterExport(cl, c("x","spd_distance_vec", "min_age", "max_age", "get_spd"))
      parallel::clusterEvalQ(cl, {
        library(tidyverse)
        library(rcarbon)
        } )
      parallel::parLapply(cl, spd_distance_vec, function(y) {
        get_spd(
          data_source_c14 = x,
          data_source_dist_vec = y,
          age_from = min_age,
          age_to = max_age,
          sel_smooth_size = 100,
          min_n_dates = 50
        )
      })
    }
  ) %>% 
  list_rbind %>% 
  parallel::stopCluster() -> data_spd

 
data_spd

#----------------------------------------------------------#
# 3. Save processed spd to data folder ------
#----------------------------------------------------------#
readr::write_rds(
  x = data_spd,
  file = paste0(
    data_storage_path,
    "Data/spd/data_spd_processed-2024-01-26.rds"
  )
)


