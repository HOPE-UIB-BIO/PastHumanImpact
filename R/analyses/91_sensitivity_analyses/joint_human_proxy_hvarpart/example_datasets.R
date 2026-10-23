#----------------------------------------------------------#
# Joint human-proxy HVarPart dataset temporal examples
#----------------------------------------------------------#

library(here)

source(here::here("R/00_Config_file.R"))

rewrite_predictions <- FALSE
max_prediction_draws <- 250L
vec_example_dataset_ids <-
  c(
    joint_human = "14944",
    climate = "15394"
  )
vec_model_variables <-
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
vec_figure_variables <-
  append(vec_model_variables, c("kk10", "hyde"), after = 1L)
vec_figure_labels <-
  resolve_temporal_variable_label(vec_figure_variables)

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")
path_model_dir <-
  file.path(path_temporal_models, "Mods")
path_prediction_dir <-
  file.path(path_temporal_models, "Core_trends")
path_joint_store <-
  file.path(
    data_storage_path,
    "Targets_data",
    "sensitivity_analyses",
    "joint_human_proxy_hvarpart"
  )
path_joint_runner <-
  here::here(
    "R",
    "analyses",
    "91_sensitivity_analyses",
    "joint_human_proxy_hvarpart",
    "00_run.R"
  )
path_figure_dir <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Sensitivity",
    "Joint_human_proxy_hvarpart",
    "Dataset_examples"
  )

run_directory_setup(path_figure_dir)


# Load canonical temporal data and joint-proxy inputs.
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
      "data_preparation",
      "paps"
    )
  )
data_raw_roc <-
  targets::tar_read_raw(
    name = "data_roc_for_modelling",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "data_preparation",
      "paps"
    )
  )
data_raw_climate <-
  targets::tar_read_raw(
    name = "data_climate_for_interpolation",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "data_preparation",
      "predictors"
    )
  )
data_raw_spd <-
  targets::tar_read_raw(
    name = "data_spd_to_fit",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "data_preparation",
      "predictors"
    )
  )

data_joint_within_dataset <-
  load_target_store_value(
    store = path_joint_store,
    target_name = "data_joint_within_dataset",
    runner = path_joint_runner
  )
data_joint_proxy_matches <-
  load_target_store_value(
    store = path_joint_store,
    target_name = "data_joint_human_proxy_matches",
    runner = path_joint_runner
  )
joint_response_variables <-
  load_target_store_value(
    store = path_joint_store,
    target_name = "joint_response_variables",
    runner = path_joint_runner
  )
joint_predictor_vars <-
  load_target_store_value(
    store = path_joint_store,
    target_name = "joint_predictor_vars",
    runner = path_joint_runner
  )


# Retain the established human- and climate-dominated example datasets.
data_selected_examples <-
  tibble::tibble(
    example_type = names(vec_example_dataset_ids),
    dataset_id = unname(vec_example_dataset_ids)
  )
data_selected_strata <-
  data_general_model |>
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]]),
    region = as.character(.data[["region"]]),
    climatezone = as.character(.data[["climatezone"]])
  ) |>
  dplyr::filter(.data[["dataset_id"]] %in% vec_example_dataset_ids) |>
  dplyr::distinct(
    .data[["dataset_id"]],
    .data[["region"]],
    .data[["climatezone"]]
  )

assertthat::assert_that(
  nrow(data_selected_strata) == length(vec_example_dataset_ids),
  !anyDuplicated(data_selected_strata[["dataset_id"]]),
  msg = "Both established examples require one temporal model stratum."
)


# Load the existing dataset-specific temporal predictions.
data_complete_models <-
  data_model_config |>
  dplyr::filter(
    .data[["analysis"]] %in% c("predictor_temporal", "pap_temporal"),
    .data[["variable"]] %in% vec_model_variables,
    .data[["is_model_eligible"]],
    !.data[["need_to_run"]],
    !.data[["need_to_be_evaluated"]]
  )

assertthat::assert_that(
  setequal(unique(data_complete_models[["variable"]]), vec_model_variables),
  msg = "All canonical temporal variables require completed eligible models."
)

data_config_selected <-
  data_complete_models |>
  dplyr::mutate(
    region = as.character(.data[["region"]]),
    climatezone = as.character(.data[["climatezone"]])
  ) |>
  dplyr::semi_join(data_selected_strata, by = c("region", "climatezone"))
data_dataset_predictions <-
  purrr::map(
    .x = data_config_selected[["model_id"]],
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
    .progress = "Loading selected dataset predictions"
  ) |>
  purrr::compact() |>
  dplyr::bind_rows() |>
  dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
  dplyr::filter(.data[["dataset_id"]] %in% vec_example_dataset_ids) |>
  prepare_region_factor() |>
  prepare_climatezone_factor() |>
  dplyr::mutate(
    variable_label = factor(
      resolve_temporal_variable_label(.data[["variable"]]),
      levels = vec_figure_labels
    )
  )

data_proxy_observed <-
  prepare_joint_human_proxy_example_trends(
    data_proxy_matches = data_joint_proxy_matches,
    data_metadata = data_meta,
    dataset_ids = unname(vec_example_dataset_ids),
    variables = c("kk10", "hyde"),
    age_min = 2000,
    age_max = 8000
  )
data_dataset_observed <-
  data_general_model |>
  dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
  dplyr::filter(
    .data[["dataset_id"]] %in% vec_example_dataset_ids,
    .data[["analysis"]] %in% c("predictor_temporal", "pap_temporal"),
    .data[["variable"]] %in% vec_model_variables
  ) |>
  dplyr::bind_rows(data_proxy_observed) |>
  prepare_region_factor() |>
  prepare_climatezone_factor() |>
  dplyr::mutate(
    variable_label = factor(
      resolve_temporal_variable_label(.data[["variable"]]),
      levels = vec_figure_labels
    )
  )
data_dataset_raw <-
  prepare_raw_temporal_data(
    data_diversity = data_raw_diversity,
    data_roc = data_raw_roc,
    data_climate = data_raw_climate,
    data_spd = data_raw_spd,
    dataset_ids = unname(vec_example_dataset_ids),
    age_min = 500,
    age_max = 8500
  ) |>
  dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
  dplyr::inner_join(
    data_meta |>
      dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
      dplyr::select(dataset_id, region, climatezone),
    by = "dataset_id"
  ) |>
  prepare_region_factor() |>
  prepare_climatezone_factor() |>
  dplyr::mutate(
    variable_label = factor(
      resolve_temporal_variable_label(.data[["variable"]]),
      levels = vec_figure_labels
    )
  )
data_dataset_metadata <-
  data_meta |>
  dplyr::mutate(dataset_id = as.character(.data[["dataset_id"]])) |>
  dplyr::filter(.data[["dataset_id"]] %in% vec_example_dataset_ids) |>
  dplyr::distinct(.data[["dataset_id"]], .keep_all = TRUE)


# Match the original figure's no-time-control HVarPart design, but use all
# three human proxies as one predictor group.
data_joint_examples <-
  data_joint_within_dataset |>
  dplyr::filter(.data[["dataset_id"]] %in% vec_example_dataset_ids)
output_joint_examples <-
  fit_hvarpart_models(
    data_source = data_joint_examples,
    response_vars = joint_response_variables,
    predictor_vars = joint_predictor_vars,
    response_dist = NULL,
    data_response_dist = NULL,
    run_all_predictors = FALSE,
    time_series = TRUE,
    get_significance = FALSE,
    permutations = 999L,
    fail_on_error = FALSE
  )
data_joint_importance <-
  compute_hvarpart_importance(
    data_source = output_joint_examples,
    id_cols = "dataset_id"
  )

assertthat::assert_that(
  all(data_joint_importance[["is_importance_eligible"]]),
  msg = "Both selected datasets require estimable joint HVarPart results."
)

readr::write_csv(
  data_selected_examples |>
    dplyr::left_join(data_joint_importance, by = "dataset_id") |>
    dplyr::mutate(
      human_predictors = "sqrt(SPD);KK10;sqrt(HYDE)",
      climate_predictors =
        "temp_annual;temp_cold;prec_summer;prec_win",
      temporal_control = FALSE,
      matched_age_min_bp = 2000,
      matched_age_max_bp = 8000
    ),
  file = file.path(
    path_figure_dir,
    "joint_human_proxies__dataset_example_selection.csv"
  )
)


# Build the same two-row composition as the canonical example figure.
vec_selected_ids <- data_selected_examples[["dataset_id"]]
list_raw_by_dataset <- split(data_dataset_raw, data_dataset_raw[["dataset_id"]])
list_observed_by_dataset <-
  split(data_dataset_observed, data_dataset_observed[["dataset_id"]])
list_predictions_by_dataset <-
  split(data_dataset_predictions, data_dataset_predictions[["dataset_id"]])
list_metadata_by_dataset <-
  split(data_dataset_metadata, data_dataset_metadata[["dataset_id"]])

data_figure_inputs <-
  data_selected_examples |>
  dplyr::transmute(
    dataset_id = .data[["dataset_id"]],
    example_type = .data[["example_type"]],
    data_raw = unname(list_raw_by_dataset[vec_selected_ids]),
    data_observed = unname(list_observed_by_dataset[vec_selected_ids]),
    data_predictions = unname(list_predictions_by_dataset[vec_selected_ids]),
    data_metadata = unname(list_metadata_by_dataset[vec_selected_ids])
  ) |>
  dplyr::mutate(
    plot = purrr::pmap(
      .l = list(data_raw, data_observed, data_predictions, data_metadata),
      .f = ~ plot_hvarpart_dataset_temporal_example(
        data_raw = ..1,
        data_observed = ..2,
        data_predictions = ..3,
        data_metadata = ..4,
        data_importance = data_joint_importance
      )
    ),
    panel_label = c(
      "A Joint-human-dominated example (SPD + KK10 + HYDE)",
      "B Climate-dominated example"
    ),
    plot_labelled = purrr::map2(
      .x = .data[["plot"]],
      .y = .data[["panel_label"]],
      .f = ~ cowplot::ggdraw() +
        cowplot::draw_plot(.x, x = 0, y = 0, width = 1, height = 0.96) +
        cowplot::draw_label(
          label = .y,
          x = 0,
          y = 1,
          hjust = 0,
          vjust = 1,
          fontface = "bold",
          size = 11
        )
    )
  )

plot_examples <-
  cowplot::plot_grid(
    plotlist = data_figure_inputs[["plot_labelled"]],
    ncol = 1,
    align = "v"
  )

purrr::walk(
  c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    filename = file.path(
      path_figure_dir,
      stringr::str_glue(
        "joint_human_proxies__dataset_temporal_examples.{.x}"
      )
    ),
    plot = plot_examples,
    width = 390,
    height = 300,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
)
