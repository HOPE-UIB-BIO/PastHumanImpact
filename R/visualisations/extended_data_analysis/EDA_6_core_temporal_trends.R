#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                 Supplementary figures
#              Core-level temporal trends
#
#                   O. Mottl, V.A. Felde
#                         2026
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here(
    "R/00_Config_file.R"
  )
)

rewrite_predictions <- FALSE
rewrite_figures <- FALSE
max_prediction_draws <- 250L
vec_requested_dataset_ids <-
  commandArgs(trailingOnly = TRUE)
# Optional IDs limit rendering after the complete prediction cache is checked.

vec_temporal_analyses <-
  c("predictor_temporal", "pap_temporal")
vec_temporal_variables <-
  c(
    "spd",
    "temp_annual",
    "temp_cold",
    "prec_summer",
    "prec_win",
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
  )
vec_temporal_labels <-
  get_temporal_variable_label(vec_temporal_variables)

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")
path_model_dir <-
  file.path(path_temporal_models, "Mods")
path_prediction_dir <-
  file.path(path_temporal_models, "Core_trends")
path_figure_dir <-
  here::here(
    "Outputs",
    "Figures",
    "Supplementary_figures",
    "Core_temporal_trends"
  )

make_dir(path_prediction_dir)
make_dir(path_figure_dir)


#----------------------------------------------------------#
# 1. Load model data and configuration -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = path_temporal_models
  )
data_model_config <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  )
data_meta <-
  RUtilpol::get_latest_file(
    file_name = "data_meta",
    dir = file.path(data_storage_path, "Assembly")
  )

data_raw_diversity <-
  targets::tar_read_raw(
    name = "data_diversity_and_dcca",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_paps"
    )
  )
data_raw_roc <-
  targets::tar_read_raw(
    name = "data_roc_for_modelling",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_paps"
    )
  )
data_raw_climate <-
  targets::tar_read_raw(
    name = "data_climate_for_interpolation",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_predictors"
    )
  )
data_raw_spd <-
  targets::tar_read_raw(
    name = "data_spd_to_fit",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "pipeline_predictors"
    )
  )

data_selected_config <-
  data_model_config %>%
  dplyr::filter(
    analysis %in% vec_temporal_analyses,
    variable %in% vec_temporal_variables,
    is_model_eligible
  )

assertthat::assert_that(
  nrow(data_selected_config) > 0L,
  all(!data_selected_config[["need_to_run"]]),
  all(!data_selected_config[["need_to_be_evaluated"]]),
  setequal(
    unique(data_selected_config[["variable"]]),
    vec_temporal_variables
  ),
  msg = "All eligible PAP and predictor models must be fitted and evaluated."
)


#----------------------------------------------------------#
# 2. Predict every fitted core -----
#----------------------------------------------------------#

list_core_predictions <-
  purrr::map(
    .x = data_selected_config[["model_id"]],
    .f = ~ predict_configured_temporal_model(
      model_id = .x,
      data_source = data_general_model,
      config_dir = path_temporal_models,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      rewrite = rewrite_predictions,
      max_prediction_draws = max_prediction_draws,
      prediction_range = "group_observed",
      prediction_estimand = "dataset_specific",
      verbose = TRUE
    ),
    .progress = "Predicting core temporal trends"
  )

data_core_predictions <-
  list_core_predictions %>%
  purrr::compact() %>%
  dplyr::bind_rows()

assertthat::assert_that(
  nrow(data_core_predictions) > 0L,
  all(
    data_core_predictions[["prediction_estimand"]] ==
      "dataset_specific"
  ),
  msg = "Core predictions must retain dataset-specific fitted trajectories."
)


#----------------------------------------------------------#
# 3. Prepare figure data -----
#----------------------------------------------------------#

data_core_observed <-
  data_general_model %>%
  dplyr::filter(
    analysis %in% vec_temporal_analyses,
    variable %in% vec_temporal_variables
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    variable_label = factor(
      get_temporal_variable_label(variable),
      levels = vec_temporal_labels
    )
  )

data_core_raw <-
  prepare_raw_temporal_data(
    data_diversity = data_raw_diversity,
    data_roc = data_raw_roc,
    data_climate = data_raw_climate,
    data_spd = data_raw_spd,
    dataset_ids = unique(
      as.character(data_core_observed[["dataset_id"]])
    ),
    age_min = 500,
    age_max = 8500
  ) %>%
  dplyr::inner_join(
    data_meta %>%
      dplyr::select(dataset_id, region, climatezone),
    by = "dataset_id"
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    variable_label = factor(
      get_temporal_variable_label(variable),
      levels = vec_temporal_labels
    )
  )

data_core_metadata <-
  data_meta %>%
  dplyr::filter(
    as.character(dataset_id) %in%
      as.character(data_core_observed[["dataset_id"]])
  ) %>%
  dplyr::distinct(dataset_id, .keep_all = TRUE)

data_core_predictions <-
  data_core_predictions %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    variable_label = factor(
      get_temporal_variable_label(variable),
      levels = vec_temporal_labels
    )
  )

vec_dataset_ids <-
  intersect(
    intersect(
      unique(as.character(data_core_observed[["dataset_id"]])),
      unique(as.character(data_core_predictions[["dataset_id"]]))
    ),
    unique(as.character(data_core_raw[["dataset_id"]]))
  ) %>%
  sort()

if (
  length(vec_requested_dataset_ids) > 0L
) {
  assertthat::assert_that(
    all(vec_requested_dataset_ids %in% vec_dataset_ids),
    msg = "Every requested dataset ID must have observations and predictions."
  )
  vec_dataset_ids <-
    vec_requested_dataset_ids
}

list_observed_by_dataset <-
  split(
    data_core_observed,
    as.character(data_core_observed[["dataset_id"]])
  )
list_raw_by_dataset <-
  split(
    data_core_raw,
    as.character(data_core_raw[["dataset_id"]])
  )
list_predictions_by_dataset <-
  split(
    data_core_predictions,
    as.character(data_core_predictions[["dataset_id"]])
  )
list_metadata_by_dataset <-
  split(
    data_core_metadata,
    as.character(data_core_metadata[["dataset_id"]])
  )

data_figure_inputs <-
  tibble::tibble(
    dataset_id = vec_dataset_ids,
    data_raw = unname(list_raw_by_dataset[vec_dataset_ids]),
    data_observed = unname(list_observed_by_dataset[vec_dataset_ids]),
    data_predictions = unname(
      list_predictions_by_dataset[vec_dataset_ids]
    ),
    data_metadata = unname(list_metadata_by_dataset[vec_dataset_ids])
  )


#----------------------------------------------------------#
# 4. Save one figure per core -----
#----------------------------------------------------------#

purrr::pwalk(
  .l = data_figure_inputs %>%
    dplyr::select(
      data_raw,
      data_observed,
      data_predictions,
      data_metadata
    ),
  .f = save_core_temporal_figure,
  output_dir = path_figure_dir,
  rewrite = rewrite_figures,
  width = 300,
  height = 160,
  dpi = 300,
  verbose = TRUE,
  .progress = "Saving core temporal figures"
)
