#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                
#                     Calculate SPD
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#--------------------------------------------------------------#

#--------------------------------------------------------------#
# 0.0. Setup -----
#--------------------------------------------------------------#

library(here)

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
  c(250) %>%
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

# library(furrr)
# 
# future::plan(
#   future::multisession(),
#   workers = parallel::detectCores()-1
# )
# 
# 
# # use future iwalk
# data_c14_subset %>%
#   split(.$dataset_id) %>%
#   rlang::set_names(data_c14_subset$dataset_id) %>%
#   furrr::future_iwalk(
#     .progress = TRUE,
#     .x = .,
#     .f = ~ {
#       
#       if(
#         !file.exists(
#           paste0(
#             data_storage_path,
#             "Data/spd/spd_test/",
#             paste0(
#               .y,
#               "_spd.rds"
#             )
#           )
#         )
#       ) {
#         
#         get_spd(
#           data_source_c14 = .,
#           data_source_dist_vec = spd_distance_vec,
#           sel_smooth_size = 100,
#           min_n_dates = 50,
#           age_from = min_age, # [config]
#           age_to = max_age # [config]
#         ) %>%
#           readr::write_rds(
#             paste0(data_storage_path,
#                    "Data/spd/spd_test/",
#                    paste0(
#                      .y,
#                      "_spd.rds"
#                    )
#             )
#           )
#         
#       }
#     }
#   )


#it works with future_map
future::plan(
  future::multicore,
  workers = parallel::detectCores()-1
)



data_c14_list <- data_c14_subset %>%
  split(.$dataset_id) 


spd <- furrr::future_map(
  .progress = TRUE,
  data_c14_list, ~get_spd(
    data_source_c14 = .x,
    data_source_dist_vec = spd_distance_vec,
    sel_smooth_size = 100,
    min_n_dates = 50,
    age_from = min_age, # [config]
    age_to = max_age # [config]
  )
) %>% 
  bind_rows()


#---------------------------------------------------------------#
# 3. Load processed spds ----
#---------------------------------------------------------------#

spd_processed_vec <-
  list.files(
    here::here(data_storage_path, "Data/spd/spd_temp"),
    pattern = "_spd.rds",
    recursive = TRUE
  )

spd_processed_list <-
  purrr::map(
    .progress = TRUE,
    .x = spd_processed_vec,
    .f = ~ readr::read_rds(
      here::here(
        data_storage_path, "Data/spd/spd_temp", .x
      )
    )
  ) %>%
  purrr::set_names(
    nm = stringr::str_replace(spd_processed_vec, "_spd.rds", "")
  )

data_spd_temp <- 
  dplyr::bind_rows(.id = "dataset_id",
                   spd_processed_list)

#---------------------------------------------------------------#
# 4. Save spd data to data folder ----
#---------------------------------------------------------------#


RUtilpol::save_latest_file(
  object_to_save = data_spd_temp,
  file_name = "data_spd",
  dir = paste0(
    data_storage_path,
    "Data/spd/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)

#---------------------------------------------------------------#