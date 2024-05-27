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

# - load data spd distance 250
data_spd_250 <-
  RUtilpol::get_latest_file(
    file_name = "data_spd",
    dir = paste0(
      data_storage_path,
      "Data/spd/"
    )
  )

# get dataset_ids for missing spds
missing_spd_250 <-
  data_spd_250 %>%
  dplyr::left_join(
    data_meta %>%
      dplyr::select(
        dataset_id,
        region,
        climatezone,
        data_publicity
      ),
    by = "dataset_id"
  ) %>%
  dplyr::filter(
    !(region == "Latin America" & data_publicity == "private"),
    !region == "Africa"
  ) %>%
  dplyr::mutate(no_spd = purrr::map_lgl(
    .x = spd,
    .f = ~ all(.x$`250` == 0)
  )) %>%
  dplyr::filter(no_spd == TRUE) %>%
  purrr::pluck("dataset_id")

#- get relvant subsets to estimate spd 500
subset_data_meta <-
  data_meta %>%
  dplyr::filter(dataset_id %in% missing_spd_250)

# - distances to calculate spd density curves
spd_distance_vec <-
  c(500) %>%
  rlang::set_names()

# - get polygons for each dataset_id
data_polygons <-
  get_polygons(
    data_source = subset_data_meta,
    distance_buffer = 10 # 10° away from site
  )

# - path for c14 data
data_c14_path <-
  paste0(
    data_storage_path,
    "Data/c14/data_rc_2022-11-29.rds"
  )
# - load c14 data
data_c14 <-
  get_file_from_path(data_c14_path)

# - subset C14 data for each dataset_id and calculate distance to it
data_c14_subset <-
  subset_c14_data(
    data_source_c14 = data_c14,
    data_source_polygons = data_polygons,
    data_source_meta = subset_data_meta
  )


#---------------------------------------------------------------#
# 2. Calculate spd for each distance per locality -----
#---------------------------------------------------------------#

# in order to several machines can work on the same data, it is better that the
#   list is in radnom order, so that the chnage that the same record  is
#   processed by different machine machine is small.

# get random seed
set.seed(Sys.time())

# get a random number sequence
random_order <-
  sample(seq_along(unique(data_c14_subset$dataset_id)))

# split data into list
data_c14_as_list <-
  data_c14_subset %>%
  split(.$dataset_id)

# reorder list
data_c14_as_list_reorder <-
  data_c14_as_list[random_order]

# get number of cores
n_cores <-
  as.numeric(
    parallelly::availableCores()
  )

# select future backend based on OS
future_backend <-
  switch(Sys.info()["sysname"],
    Windows = "future::multisession",
    Linux = "future::multicore"
  )

# set future plan
future::plan(
  future_backend,
  workers = n_cores - 1
)

# use future iwalk
data_c14_as_list_reorder %>%
  furrr::future_iwalk(
    .progress = TRUE,
    .x = .,
    .f = ~ {
      # output the records dataset_id to tract progress
      message(.y)
      if (
        !file.exists(
          paste0(
            data_storage_path,
            "Data/spd/spd_temp_500/",
            paste0(
              .y,
              "_spd.rds"
            )
          )
        )
      ) {
        get_spd(
          data_source_c14 = .x,
          data_source_dist_vec = spd_distance_vec,
          sel_smooth_size = 100,
          min_n_dates = 50,
          age_from = min_age, # [config]
          age_to = max_age # [config]
        ) %>%
          # in case that several machines estimate the same record at the same
          #  time, we use the sha to avoid overwriting the file
          RUtilpol::save_latest_file(
            object_to_save = .,
            dir = paste0(
              data_storage_path,
              "Data/spd/spd_temp_500/"
            ),
            file_name = paste0(
              .y,
              "_spd.rds"
            ),
            prefered_format = "rds"
          )
      }
    }
  )

#---------------------------------------------------------------#
# 3. Load processed spds ----
#---------------------------------------------------------------#

spd_processed_vec <-
  list.files(
    here::here(data_storage_path, "Data/spd/spd_temp_500"),
    pattern = "_spd.rds",
    recursive = TRUE
  ) %>%
  purrr::map(
    .f = ~ RUtilpol::get_clean_name(.x)
  ) %>%
  unique()

spd_processed_list <-
  purrr::map(
    .progress = TRUE,
    .x = spd_processed_vec,
    .f = ~ RUtilpol::get_latest_file(
      file_name = .x,
      dir = here::here(
        data_storage_path, "Data/spd/spd_temp_500"
      )
    )
  ) %>%
  purrr::set_names(
    nm = stringr::str_remove(
      spd_processed_vec,
      "_spd.rds"
    )
  )

data_spd_temp <-
  dplyr::bind_rows(
    .id = "dataset_id",
    spd_processed_list
  )

#---------------------------------------------------------------#
# 4. Save spd data to data folder ----
#---------------------------------------------------------------#

RUtilpol::save_latest_file(
  object_to_save = data_spd_temp,
  file_name = "data_spd_500",
  dir = paste0(
    data_storage_path,
    "Data/spd/"
  ),
  prefered_format = "rds",
  use_sha = FALSE
)

#---------------------------------------------------------------#
