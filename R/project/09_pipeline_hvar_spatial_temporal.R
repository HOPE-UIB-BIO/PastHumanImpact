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
      "_targets_data/pipeline_paps/objects/data_properties_filtered"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_properties_filtered,
    command = get_file_from_path(data_properties_path)
  ),
  # load data_predictors ----
  targets::tar_target(
    name = data_predictor_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_predictors/objects/data_predictors_filtered"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_predictors_filtered,
    command = get_file_from_path(data_predictor_path)
  ),
  # - combine properties and predictors for hvar ----
  targets::tar_target(
    name = data_hvar_filtered,
    command = get_data_combined(
      data_source_properties = data_properties_filtered,
      data_source_predictors = data_predictors_filtered
    )
  ),
  # - get data for timebins ----
  targets::tar_target(
    name = data_hvar_timebins,
    command = get_data_timebin(
      data_source = data_hvar_filtered,
      data_meta = data_meta
    )
  ),
  # - Hierarchical variation partitioning: ----
  # - run spatial (within core) analysis with spd ----
  targets::tar_target(
    name = output_spatial_spd,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
      response_dist = NULL,
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
      data_source = data_hvar_filtered,
      response_dist = NULL,
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
      data_source = data_hvar_timebins,
      response_dist = NULL,
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
      data_source = data_hvar_timebins,
      response_dist = NULL,
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
  ),
  # - summary tables with human = spd as predictor ----
  targets::tar_target(
    name = summary_tables_spd,
    command = get_summary_tables(
      output_spatial = output_spatial_spd,
      output_temporal = output_temporal_spd,
      data_meta = data_meta
    )
  ),
  # - summary tables with human = events as predictor ----
  targets::tar_target(
    name = summary_tables_events,
    command = get_summary_tables(
      output_spatial = output_spatial_events,
      output_temporal = output_temporal_events,
      data_meta = data_meta
    )
  )
)

