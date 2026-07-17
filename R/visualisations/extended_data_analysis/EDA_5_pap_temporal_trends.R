#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    Extended data
#                  PAP temporal trends
#
#                   V. Felde, O. Mottl
#                         2024
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

vec_all_paps <-
  c(
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

# Select a subset here when preparing the final primary figure.
vec_primary_paps <-
  vec_all_paps

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")
path_prediction_dir <-
  file.path(path_temporal_models, "General_trends")
path_figure_dir <-
  here::here("Outputs", "Figures", "Extended_data_figures")

make_dir(path_figure_dir)


#----------------------------------------------------------#
# 1. Load and validate data -----
#----------------------------------------------------------#

data_pap_observed <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = path_temporal_models
  ) %>%
  dplyr::filter(analysis == "pap_temporal") %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    pap_label = get_temporal_variable_label(variable),
    pap_label = factor(
      pap_label,
      levels = get_temporal_variable_label(vec_all_paps)
    )
  )

data_pap_predictions <-
  RUtilpol::get_latest_file(
    file_name = "pap_temporal_model_predictions",
    dir = path_prediction_dir
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    pap_label = get_temporal_variable_label(variable),
    pap_label = factor(
      pap_label,
      levels = get_temporal_variable_label(vec_all_paps)
    )
  )

assertthat::assert_that(
  setequal(unique(data_pap_predictions[["variable"]]), vec_all_paps),
  all(data_pap_predictions[["prediction_range"]] == "group_observed"),
  all(data_pap_predictions[["n_datasets_marginalised"]] > 0L),
  msg = "PAP predictions must cover all PAPs over observed core ranges."
)

data_prediction_strata <-
  data_pap_predictions %>%
  dplyr::distinct(variable, region, climatezone)
data_observed_strata <-
  data_pap_observed %>%
  dplyr::distinct(variable, region, climatezone)

assertthat::assert_that(
  nrow(data_prediction_strata) == length(vec_all_paps) * 31L,
  nrow(data_prediction_strata) == nrow(data_observed_strata),
  msg = "PAP predictions must cover all 31 observed strata per PAP."
)


#----------------------------------------------------------#
# 2. Build the primary figure -----
#----------------------------------------------------------#

plot_pap_primary <-
  save_pap_temporal_figure(
    data_observed = data_pap_observed,
    data_predictions = data_pap_predictions,
    pap_variables = vec_primary_paps,
    layout = "primary",
    output_dir = path_figure_dir,
    output_stem = "PAP_through_time_primary",
    width = 270,
    height = 70 + 28 * length(vec_primary_paps)
  )


#----------------------------------------------------------#
# 3. Build all supplementary figures -----
#----------------------------------------------------------#

purrr::walk(
  vec_all_paps,
  .f = ~ save_pap_temporal_figure(
    data_observed = data_pap_observed,
    data_predictions = data_pap_predictions,
    pap_variables = .x,
    layout = "strata",
    output_dir = path_figure_dir,
    output_stem = paste0("PAP_through_time_", .x),
    width = 270,
    height = 200
  )
)
