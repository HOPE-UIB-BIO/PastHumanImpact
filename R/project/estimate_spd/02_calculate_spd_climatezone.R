#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                
#                 Calculate SPD for CLIMATE ZONE
#
#                   O. Mottl, V.A. Felde
#                         2023
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
# spd_distance_vec <- 
#   c(250) %>%
#   rlang::set_names()

# # - get polygons for each dataset_id
# data_polygons <-
#   get_polygons(
#     data_source = data_meta,
#     distance_buffer = 10 # 10° away from site
#   )

# - path for c14 data
data_c14_path <- 
  paste0(
    data_storage_path,
    "Data/c14/data_rc_2022-11-29.rds"
  )
# - load c14 data
data_c14 <- get_file_from_path(data_c14_path)

# select distinct data
data_rc_work <-
  data_c14 %>%
  dplyr::distinct(LabID, .keep_all = TRUE) %>%
  dplyr::select(
    LabID,
    lat, long,
    Age, Error, Continent
  )


# assign raster values from file
data_rc_work <- RUtilpol::geo_assign_value(
  data_source = data_rc_work,
  dir = paste0(
    data_storage_path,
    "Data/ecoregions2017/"),
  sel_method = "tif",
  file_name = "Beck_KG_V1_present_0p083",
  tif_fill_na = TRUE,
  tif_distance_step = 5e3,
  tif_n_max_step = 20
)

#translate raster values to climatezones
koppen_translation_table <-
  readr::read_csv(
    paste0(data_storage_path, 
           "/Data/ecoregions2017/koppen_link.csv"))

data_rc_work <-
  data_rc_work %>% 
  dplyr::left_join(
    koppen_translation_table,
    by = "raster_values"
    ) %>% 
  dplyr::select(-raster_values) %>% 
  dplyr::rename(
    ecozone_koppen_30 = genzone,
    ecozone_koppen_15 = genzone_cluster,
    ecozone_koppen_5 = broadbiome) %>%
  dplyr::mutate(
    climatezone = dplyr::case_when(
      ecozone_koppen_15 == "Cold_Without_dry_season" ~ ecozone_koppen_30,
      ecozone_koppen_5 == "Cold" ~ ecozone_koppen_15,
      ecozone_koppen_5 == "Temperate" ~ ecozone_koppen_15,
      .default = ecozone_koppen_5
    )
  ) %>%
  dplyr::select(
    -c(ecozone_koppen_30, 
       ecozone_koppen_15)
    )

sf::sf_use_s2(FALSE)  

# add curve name assign by shapefile - take time
data_rc_work <- RUtilpol::geo_assign_value(
  data_source = data_rc_work,
  dir = paste0(
    data_storage_path,
    "Data/Calibration_curves_shapefile/"),
  sel_method = "shapefile",
  file_name = "Calibration_curves",
  var = "curve_name"
)

  
data_rc_climatezone <- data_rc_work %>%
  dplyr::filter(!Continent == "Africa") %>%
  nest(rc_data = -c(Continent, climatezone))




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
  data_c14_list, ~get_spd(
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