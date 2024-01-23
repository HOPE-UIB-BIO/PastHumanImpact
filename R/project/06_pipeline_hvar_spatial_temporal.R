#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                      Hypothesis I
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

# Load configuration
source(
  here::here(
    "R/00_Config_file.R"
  )
)

Sys.setenv(TAR_PROJECT = "project")

targets::tar_option_set(
  packages = package_list, # [config]
  memory = "transient",
  garbage_collection = TRUE,
  repository = "local",
  seed = set_seed, # [config]
  storage = "worker"
)

#----------------------------------------------------------#
# 1. Targets -----
#----------------------------------------------------------#

list(
  # 1. Load climate data ----
  targets::tar_target(
    name = file_climate_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/climate/data_climate-2024-01-23.rds"
    ),
    format = "file"
  ),
  # - load data from path
  targets::tar_target(
    name = data_climate,
    command = get_data(file_climate_path)
  ),
  # - select climate variables
  targets::tar_target(
    name = data_climate_for_interpolation,
    command = get_climate_data_for_interpolation(
      data_source = data_climate,
      sel_var = c(
        "temp_annual",
        "temp_cold",
        "prec_annual",
        "prec_summer",
        "prec_win"
      )
    )
  ),
  # - interpolate climate data for each time slice
  targets::tar_target(
    name = data_climate_interpolated,
    command = get_interpolated_data(
      data_source = data_climate_for_interpolation,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # 2. Load spd data ----
  targets::tar_target(
    name = file_spd_path,
    command = paste0(
      data_storage_path,
      "HOPE_Hypothesis1/Data/spd/data_spd_processed-2024-01-23.rds"
    ),
    format = "file"
  ),
  # - load data from path
  targets::tar_target(
    name = data_spd,
    command = get_data(file_spd_path)
  ),
  # - prepare spd for modelling
  targets::tar_target(
    name = data_spd_to_fit,
    command = get_spd_for_modelling(data_spd)
  ),
  # - get interpolated spd values for each time slice
  targets::tar_target(
    name = data_spd_interpolated,
    command = get_interpolated_data(
      data_source = data_spd_to_fit,
      variable = "var_name",
      vars_interpolate = c("age", "value"),
      group_var = "dataset_id",
      method = "linear",
      rule = 1,
      ties = mean,
      age_min = 0,
      age_max = 12e03,
      timestep = 500,
      verbose = TRUE
    )
  ),
  # - combine spd and human impact data ---- 
  targets::tar_target(
    name = data_spd_events,
    command = get_events_spd_combined(
      data_source_events = events_temporal_subset,
      data_source_spd = data_spd_interpolated,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time,
      select_spd_distance = 250
    )
  ),
  # - combine all data together 
  targets::tar_target(
    name = data_all_combined,
    command = get_data_for_hvarpar(
      data_source_diversity = data_div_dcca_interpolated,
      data_source_roc = data_roc_interpolated,
      data_source_density = data_density_estimate,
      data_source_spd = data_spd_events,
      data_source_climate = data_climate_interpolated,
      used_rescale = TRUE
    )
  ),
  # - filter data by age ranges, remove private data
  targets::tar_target(
    name = data_hvar_filtered,
    command = get_data_hvar_filtered(
      data_source = data_all_combined,
      age_min = 2000,
      age_max = 8500,
      remove_private = TRUE
    )
  ),
  # 3. Hierarchical variation partitioning 
  # - run spatial (within core) analysis with spd
  targets::tar_target(
    name = output_spatial_spd,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
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
  # - run spatial (within core) analysis with events
  targets::tar_target(
    name = output_spatial_events,
    command = run_hvarpart(
      data_source = data_hvar_filtered,
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
  # - run temporal analysis (by age bins in spatial context) with spd
  targets::tar_target(
    name = output_temporal_spd,
    command = run_hvarpart(
      data_source = data_hvar_temporal,
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
  # - run temporal analysis (by age bins in spatial context) with events
  targets::tar_target(
    name = output_temporal_events,
    command = run_hvarpart(
      data_source = data_hvar_temporal,
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

