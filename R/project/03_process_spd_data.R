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

library(furrr)

future::plan(
  future::multisession(),
  workers = parallel::detectCores()-1
)



# data_spd <-
#   tidyr::expand_grid(
#     dataset_id = unique(data_c14_subset$dataset_id)
#   ) %>%
#   dplyr::mutate(
#     spd = furrr::future_map(
#       .progress = TRUE,
#       .x = dataset_id,
#       .f = ~ get_spd(
#         data_source_c14 = data_c14_subset,
#         data_source_dist_vec = 250,
#         sel_smooth_size = 100,
#         min_n_dates = 50,
#         age_from = min_age, # [config]
#         age_to = max_age # [config]
#       )
#     )
#   )


# use future iwalk
data_c14_subset %>%
  split(.$dataset_id) %>%
  rlang::set_names(data_c14_subset$dataset_id) %>%
  furrr::future_iwalk(
    .progress = TRUE,
    .x = .,
    .f = ~ {
      
      if(
        !file.exists(
          paste0(
            data_storage_path,
            "Data/spd/processed_spd/",
            paste0(
              .y,
              "_spd.rds"
            )
          )
        )
      ) {
        
        get_spd(
          data_source_c14 = .,
          data_source_dist_vec = 250,
          sel_smooth_size = 100,
          min_n_dates = 50,
          age_from = min_age, # [config]
          age_to = max_age # [config]
        ) %>%
          readr::write_rds(
            paste0(data_storage_path,
              "Data/spd/processed_spd/",
              paste0(
                .y,
                "_spd.rds"
              )
            )
          )
        
      }
    }
  )


