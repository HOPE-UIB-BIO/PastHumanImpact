#--------------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#                    
#                
#           Revision H1 with data spd = TRUE
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

# Load spd data
data_spd_250_to_fit <- 
  targets::tar_read(
    name = "data_spd_to_fit",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors"
    )
  )

# - Load data paps
data_properties <-
  targets::tar_read(
    name = "data_properties",
    store = paste0(
      data_storage_path,
      "_targets_data/pipeline_paps"
    )
  )


# - Load data predictors
data_predictors <- targets::tar_read(
  name = "data_predictors",
  store = paste0(
    data_storage_path,
    "_targets_data/pipeline_predictors"
  )
)


#--------------------------------------------------------------#
# 1. filter data with spd
#--------------------------------------------------------------#

# get summary data with spd and no spd ----
data_summary_spd <-
  data_spd_250_to_fit %>% 
  pluck("data_to_fit") %>% 
  pluck(1) %>%
  left_join(data_meta %>% 
              dplyr::select(dataset_id, region, climatezone, data_publicity),
            by = "dataset_id") %>%
  filter(!(region == "Latin America" & data_publicity == "private"),
         !region == "Africa") %>%
  nest(spd = c(age, value)) %>%
  mutate(have_spd_250 = purrr::map_lgl(spd, 
                                       .f = ~ any(.x$value > 0)
  )
  ) %>%
  mutate(no_spd = purrr::map_lgl(spd, 
                                 .f = ~ all(.x$value == 0)
  )
  )

datasets_with_spd <- data_summary_spd %>%
  dplyr::filter(have_spd_250 == TRUE) %>%
  pluck("dataset_id")

datasets_with_spd %>% length()  

# filter data paps
data_properties_filtered <-
  get_data_filtered(
    data_source = data_properties,
    data_meta = data_meta,
    age_from = 0,
    age_to = 8500,
    remove_private = TRUE
  )  %>%
  dplyr::filter(dataset_id %in% datasets_with_spd)

# filter predictors ----
data_predictors_filtered <-
  get_data_filtered(
    data_source = data_predictors,
    data_meta = data_meta,
    age_from = 0,
    age_to = 8500,
    remove_private = TRUE
  ) %>%
  dplyr::filter(dataset_id %in% datasets_with_spd)

# combine prorperties and predictors ----
data_hvar_temporal <- 
  get_data_combined(
  data_source_properties = data_properties_filtered,
  data_source_predictors = data_predictors_filtered
  )

# reshape data for time slices ----
data_hvar_timebins <- get_data_timebin(
  data_source = data_hvar_temporal,
  data_meta = data_meta
)

# run H1 temporal ----
results_temporal_spd <- 
  run_hvarpart(
  data_source = data_hvar_timebins,
  response_dist = NULL,
  data_response_dist = NULL,
  response_vars = c(
    "n0",
    "n1",
    "n2",
    "n1_minus_n2",
    "n2_divided_by_n1",
    "n1_divided_by_n0",
    "roc",
    "dcca_axis_1",
    "density_diversity",
    "density_turnover"
  ),
  predictor_vars = list(
    human = c("spd"),
    climate = c(
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    )
  ),
  run_all_predictors = FALSE,
  time_series = FALSE,
  get_significance = FALSE,
  permutations = 999
)

# get summary output temporal ----
summary_table_temporal <- 
  get_summary_tables(
  data_source = results_temporal_spd,
  data_type = "temporal",
  group_var = "region")



