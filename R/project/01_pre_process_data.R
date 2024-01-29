#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                   1. Filter meta data
#                   2. Download climate data
#                   3. Calculate SPD
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

#---------------------------------------------------------------#
# 1.1. Filter meta data -----
#---------------------------------------------------------------#

# get path to the data assembly
file_assembly_path <-
  paste0(
    data_storage_path,
    "Data/assembly/data_assembly_V2-2022-05-23.rds"
  )
# - load data assembly from path
data_assembly <-
  get_data(file_assembly_path)

# - filter pollen data
data_assembly_filtered <- 
  filter_all_data(data_assembly)


# - select meta data for filtered dataset_id
data_meta <-
  get_meta_data(
    data_assembly = data_assembly_filtered,
    variables = c(
      "dataset_id",
      "handle",
      "country",
      "long",
      "lat",
      "altitude",
      "depositionalenvironment",
      "region",
      "curve_name",
      "ecozone_koppen_5",
      "ecozone_koppen_15",
      "ecozone_koppen_30",
      "data_publicity",
      "doi"
    )
  )

#---------------------------------------------------------------#
# 1.2. Save processed meta data to data folder -----
#---------------------------------------------------------------#

readr::write_rds(
  x = data_meta,
  file = paste0(
    data_storage_path,
    "Data/assembly/data_meta-2024-01-30.rds"
  )
)

#---------------------------------------------------------------#
# 2.1. Download CHELSA palaeoclimate data -----
#---------------------------------------------------------------#

# - a path to time reference table (from code)
time_ref_path <-
  paste0(
    data_storage_path,
    "Data/climate/time_reference_table.rds"
  )
# - load table
time_ref_table <- get_file_from_path(time_ref_path)

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


#---------------------------------------------------------------#
# 2.2. Save palaeoclimate data to data folder -----
#---------------------------------------------------------------#

readr::write_rds(
  x = data_climate,
  file = paste0(
    data_storage_path,
    "Data/climate/data_climate-2024-01-23.rds"
  )
)

#---------------------------------------------------------------#
# 3.1. Load and prepare subsets of C14 dates -----
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
# 3.2. Calculate spd for each distance per locality -----
#---------------------------------------------------------------#

library(furrr)

future::plan(
  future::multisession(),
  workers = parallel::detectCores()-1
)

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

#---------------------------------------------------------------#
# 3.3. Load processed spds ----
#---------------------------------------------------------------#

spd_processed_vec <-
  list.files(
    here::here(data_storage_path, "Data/spd/processed_spd"),
    pattern = "_spd.rds",
    recursive = TRUE
  )

spd_processed_list <-
  purrr::map(
    .progress = TRUE,
    .x = spd_processed_vec,
    .f = ~ readr::read_rds(
      here::here(
        data_storage_path, "Data/spd/processed_spd", .x
      )
    )
  ) %>%
  purrr::set_names(
    nm = stringr::str_replace(spd_processed_vec, "_spd.rds", "")
  )

data_spd <- 
  dplyr::bind_rows(.id = "dataset_id",
                   spd_processed_list)

#---------------------------------------------------------------#
# 3.4. Save spd data to data folder ----
#---------------------------------------------------------------#

readr::write_rds(
  x = data_spd,
  file = paste0(
    data_storage_path,
    "Data/spd/data_spd-2024-01-29.rds"
  )
)

#---------------------------------------------------------------#