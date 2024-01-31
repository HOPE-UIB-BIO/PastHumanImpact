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



#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

list(
  # load data_properties ----
  targets::tar_target(
    name = data_properties_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_paps/objects/data_properties"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_properties,
    command = get_file_from_path(data_properties_path)
  ),
  # load data_predictors ----
  targets::tar_target(
    name = data_predictor_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors/objects/data_predictors"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_predictors,
    command = get_file_from_path(data_predictors_path)
  ),
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
      data_source_combined = data_combined,
      data_source_meta = data_meta,
      age_from = 2000,
      age_to = 8500,
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

