#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#               Calculate pollen assemblage properties
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

# the targets list:
list(
  # get pollen data from _targets_data ----
  targets::tar_target(
    name = data_pollen_path,
    command = paste0(
      data_storage_path,
      "_targets_data/pipeline_pollen_data/objects/data_pollen"
    ),
    format = "file"
  ),
  targets::tar_target(
    name = data_pollen,
    command = get_file_from_path(data_pollen_path)
  ),
  # 3. Estimate PAPs -----
  # - calculate diversity
  targets::tar_target(
    name = data_diversity,
    command = get_diversity(
      data_pollen,
      n_rand = 999,
      sel_method = "taxonomic"
    )
  ),
  # - run detrended canonical correspondence analysis (DCCA) to estimate
  #     compositional turnover
  # - use percentages without prior transformations
  targets::tar_target(
    name = data_dcca,
    command = get_dcca(
      data_pollen,
      sel_method = "constrained",
      var_name_pred = "age",
      sel_complexity = "poly_2",
      transform_to_percentage = FALSE,
      tranformation = "none"
    )
  ),
  # - calculate Rate-of-change (RoC)
  targets::tar_target(
    name = data_roc,
    command = get_roc(
      data_pollen,
      smoothing_method = "age.w",
      min_points_smoothing = 5,
      max_points_smoothing = 9,
      age_range_smoothing = 500,
      working_units_selection = "MW",
      size_of_bin = 500,
      n_mowing_windows = 5,
      which_level_select_in_bin = "random",
      n_rand = 1000,
      n_individuals_to_standardise = 150,
      transformation_coef = "chisq",
      peak_point_method = "trend_non_linear",
      sd_for_peak_detection = 2
    )
  ),
  # - run multivariate regression trees (MRT) to estimate compositional change
  # - use percentages without prior transformation
  targets::tar_target(
    name = data_mrt,
    command = get_mrt(
      data_pollen,
      n_rand = 999,
      transformation_coef = "chisq"
    )
  ),
  # - combine all PAP estimates into one tibble for get change-points
  targets::tar_target(
    name = data_prepared_cp,
    command = prepare_data_cp(
      data_pollen,
      data_diversity,
      data_mrt,
      data_roc,
      data_dcca
    )
  ),
  # - calculate change points of all PAP variables by regression trees (RT)
  targets::tar_target(
    name = data_change_points,
    command = get_change_points_pap(data_prepared_cp)
  ),
  # - calculate density of change points
  targets::tar_target(
    name = data_density_estimate,
    command = get_density_pap_combined(
      data_source_change_points = data_change_points,
      data_source_meta = data_meta,
      data_source_dummy_time = data_dummy_time,
      limit_length = TRUE
    )
  ),
  # 4. Combine PAP data -----
  # - merge diversity and DCCA and prepare for modelling
  targets::tar_target(
    name = data_diversity_and_dcca,
    command = get_diversity_and_dcca_for_modelling(
      data_source_diversity = data_diversity,
      data_source_dcca = data_dcca,
      data_source_pollen = data_pollen
    )
  ),
  # - estimate diversity and DCCA on equal time slices 
  targets::tar_target(
    name = data_div_dcca_interpolated,
    command = get_interpolated_data(
      data_source = data_diversity_and_dcca,
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
  # - prepare RoC for modelling
  targets::tar_target(
    name = data_roc_for_modelling,
    command = get_roc_for_modelling(data_roc)
  ),
  # - estimate RoC on equal time slices 
  targets::tar_target(
    name = data_roc_interpolated,
    command = get_interpolated_data(
      data_source = data_roc_for_modelling,
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
  # - merge PAPs together
  targets::tar_target(
    name = data_properties,
    command = get_data_properties(
      data_source_diversity = data_div_dcca_interpolated,
      data_source_roc = data_roc_interpolated,
      data_source_density = data_density_estimate,
      used_rescale = TRUE)
  )
) # end of targets
