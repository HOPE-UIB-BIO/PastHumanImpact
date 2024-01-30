#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                 Run analyses for Hypothesis I
#
#
#                   O. Mottl, V.A. Felde
#                         2023
#
#----------------------------------------------------------#



#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

# - Load configuration
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

# - Setting for targets
Sys.setenv(TAR_PROJECT = "_targets_h1")

targets::tar_option_set(
  packages = package_list, # [config]
  memory = "transient",
  garbage_collection = TRUE,
  repository = "local",
  seed = set_seed, # [config]
  storage = "worker"
)

# - Load data from _targets_data

data_predictors <-   
  targets::tar_read(
  name = "data_predictors",
  store = paste0(
    data_storage_path,
    "_targets_data"
  )
)

data_properties <-   
  targets::tar_read(
    name = "data_properties",
    store = paste0(
      data_storage_path,
      "_targets_data"
    )
  )

#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

list(
  # - combine properties and predictors ----
  targets::tar_target(
    name = data_combined,
    command = get_data_combined(
      data_source_properties = data_properties,
      data_source_predictors = data_predictors
    )
  ),
  # - filter data for analyses ----
  targets::tar_target(
    name = data_records,
    command = get_data_filtered(
      data_source = data_combined,
      age_min = 2000,
      age_max = 8500,
      remove_private = TRUE
    )
  ),
  # - get data for timebins ----
  targets::tar_target(
    name = data_timebins,
    command = get_data_timebin(
      data_source = data_records_spatial,
      data_meta = data_meta
    )
  ),
  # - Hierarchical variation partitioning: ----
  # - run spatial (within core) analysis with spd ----
  targets::tar_target(
    name = output_spatial_spd,
    command = run_hvarpart(
      data_source = data_records,
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
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999
    )
  ),
  # - run spatial (within core) analysis with events ----
  targets::tar_target(
    name = output_spatial_events,
    command = run_hvarpart(
      data_source = data_records,
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
        human =  human = c(
          "fi", 
          "fc", 
          "ec", 
          "cc",
          "es",
          "ei",
          "weak", 
          "medium", 
          "strong"
        ),
        climate = c(
          "temp_annual",
          "temp_cold",
          "prec_summer",
          "prec_win"
        ),
        time = c("age")
      ),
      run_all_predictors = FALSE,
      time_series = TRUE,
      get_significance = FALSE,
      permutations = 999
    )
  ),
  # - run temporal analysis with spd ----
  targets::tar_target(
    name = output_temporal_spd,
    command = run_hvarpart(
      data_source = data_timebins,
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
  ),
  # - run temporal analysis with events ----
  targets::tar_target(
    name = output_temporal_events,
    command = run_hvarpart(
      data_source = data_timebins,
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
        human = c(
          "fi", 
          "fc", 
          "ec", 
          "cc",
          "es",
          "ei",
          "weak", 
          "medium", 
          "strong"
        ),
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
  )
)

