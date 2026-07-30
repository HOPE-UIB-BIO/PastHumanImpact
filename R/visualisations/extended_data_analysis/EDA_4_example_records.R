#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                 Supplementary figures
#            HVarPart core temporal examples
#
#                   V. Felde, O. Mottl
#                         2026
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

library(here)

source(
  here::here("R/00_Config_file.R")
)

rewrite_predictions <- FALSE
max_prediction_draws <- 250L
min_n_age_points <- 8L
min_total_explained_variation <- 0.1
vec_example_dataset_ids <-
  c(
    human = "14944",
    climate = "15394"
  )

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
    "Core_examples"
  )

make_dir(path_figure_dir)


#----------------------------------------------------------#
# 1. Load model and HVarPart data -----
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
data_hvarpart <-
  targets::tar_read(
    name = "output_spatial_spd",
    store = file.path(
      data_storage_path,
      "Targets_data",
      "analyses_h1"
    )
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


#----------------------------------------------------------#
# 2. Select contrasting complete cores -----
#----------------------------------------------------------#

data_hvarpart_importance <-
  get_hvarpart_importance(
    data_source = data_hvarpart,
    id_cols = "dataset_id"
  )
data_temporal_coverage <-
  data_general_model %>%
  dplyr::filter(
    .data[["analysis"]] %in% vec_temporal_analyses,
    .data[["variable"]] %in% vec_temporal_variables
  ) %>%
  dplyr::group_by(.data[["dataset_id"]]) %>%
  dplyr::summarise(
    n_variables = dplyr::n_distinct(.data[["variable"]]),
    n_age_points = dplyr::n_distinct(.data[["age"]]),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]])
  )
data_hvarpart_coverage <-
  data_hvarpart_importance %>%
  dplyr::group_by(.data[["dataset_id"]]) %>%
  dplyr::summarise(
    has_human = "human" %in% .data[["predictor"]],
    has_climate = "climate" %in% .data[["predictor"]],
    total_adjusted_r_squared = mean(
      .data[["total_adjusted_r_squared"]]
    ),
    importance_in_display_range = all(
      dplyr::between(.data[["individual_percent"]], 0, 100)
    ),
    .groups = "drop"
  ) %>%
  dplyr::filter(
    .data[["has_human"]],
    .data[["has_climate"]],
    .data[["importance_in_display_range"]],
    .data[["total_adjusted_r_squared"]] >=
      min_total_explained_variation
  )
data_complete_models <-
  data_model_config %>%
  dplyr::filter(
    .data[["analysis"]] %in% vec_temporal_analyses,
    .data[["variable"]] %in% vec_temporal_variables,
    .data[["is_model_eligible"]],
    !.data[["need_to_run"]],
    !.data[["need_to_be_evaluated"]]
  )

assertthat::assert_that(
  setequal(
    unique(data_complete_models[["variable"]]),
    vec_temporal_variables
  ),
  msg = "All temporal variables require completed eligible models."
)

vec_available_dataset_ids <-
  data_temporal_coverage %>%
  dplyr::filter(
    .data[["n_variables"]] == length(vec_temporal_variables),
    .data[["n_age_points"]] >= min_n_age_points
  ) %>%
  dplyr::inner_join(
    data_hvarpart_coverage %>%
      dplyr::select(dataset_id),
    by = "dataset_id"
  ) %>%
  dplyr::pull(.data[["dataset_id"]])

assertthat::assert_that(
  all(unname(vec_example_dataset_ids) %in% vec_available_dataset_ids),
  msg = "The established HVarPart example cores require complete inputs."
)

data_selected_examples <-
  tibble::tibble(
    example_type = names(vec_example_dataset_ids),
    dataset_id = unname(vec_example_dataset_ids)
  ) %>%
  dplyr::left_join(
    data_hvarpart_importance,
    by = c(
      "dataset_id",
      "example_type" = "predictor"
    )
  ) %>%
  dplyr::left_join(
    data_temporal_coverage,
    by = "dataset_id"
  )

data_selected_strata <-
  data_general_model %>%
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]]),
    region = as.character(.data[["region"]]),
    climatezone = as.character(.data[["climatezone"]])
  ) %>%
  dplyr::filter(
    .data[["dataset_id"]] %in%
      data_selected_examples[["dataset_id"]]
  ) %>%
  dplyr::distinct(
    .data[["dataset_id"]],
    .data[["region"]],
    .data[["climatezone"]]
  )
data_selected_examples <-
  data_selected_examples %>%
  dplyr::left_join(
    data_selected_strata,
    by = "dataset_id"
  )

readr::write_csv(
  x = data_selected_examples,
  file = file.path(
    path_figure_dir,
    "HVarPart_core_temporal_example_selection.csv"
  )
)


#----------------------------------------------------------#
# 3. Prepare current temporal predictions -----
#----------------------------------------------------------#

data_config_selected <-
  data_complete_models %>%
  dplyr::mutate(
    region = as.character(.data[["region"]]),
    climatezone = as.character(.data[["climatezone"]])
  ) %>%
  dplyr::semi_join(
    data_selected_strata,
    by = c("region", "climatezone")
  )

list_core_predictions <-
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
    .progress = "Loading selected core predictions"
  )

data_core_predictions <-
  list_core_predictions %>%
  purrr::compact() %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]])
  ) %>%
  dplyr::filter(
    .data[["dataset_id"]] %in%
      data_selected_examples[["dataset_id"]]
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    variable_label = factor(
      get_temporal_variable_label(.data[["variable"]]),
      levels = vec_temporal_labels
    )
  )
data_core_observed <-
  data_general_model %>%
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]])
  ) %>%
  dplyr::filter(
    .data[["dataset_id"]] %in%
      data_selected_examples[["dataset_id"]],
    .data[["analysis"]] %in% vec_temporal_analyses,
    .data[["variable"]] %in% vec_temporal_variables
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    variable_label = factor(
      get_temporal_variable_label(.data[["variable"]]),
      levels = vec_temporal_labels
    )
  )
data_core_raw <-
  prepare_raw_temporal_data(
    data_diversity = data_raw_diversity,
    data_roc = data_raw_roc,
    data_climate = data_raw_climate,
    data_spd = data_raw_spd,
    dataset_ids = data_selected_examples[["dataset_id"]],
    age_min = 500,
    age_max = 8500
  ) %>%
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]])
  ) %>%
  dplyr::inner_join(
    data_meta %>%
      dplyr::mutate(
        dataset_id = as.character(.data[["dataset_id"]])
      ) %>%
      dplyr::select(
        dataset_id,
        region,
        climatezone
      ),
    by = "dataset_id"
  ) %>%
  add_region_as_factor() %>%
  add_climatezone_as_factor() %>%
  dplyr::mutate(
    variable_label = factor(
      get_temporal_variable_label(.data[["variable"]]),
      levels = vec_temporal_labels
    )
  )
data_core_metadata <-
  data_meta %>%
  dplyr::mutate(
    dataset_id = as.character(.data[["dataset_id"]])
  ) %>%
  dplyr::filter(
    .data[["dataset_id"]] %in%
      data_selected_examples[["dataset_id"]]
  ) %>%
  dplyr::distinct(.data[["dataset_id"]], .keep_all = TRUE)

assertthat::assert_that(
  setequal(
    unique(data_core_predictions[["dataset_id"]]),
    data_selected_examples[["dataset_id"]]
  ),
  msg = "Both selected cores require current dataset-specific predictions."
)


#----------------------------------------------------------#
# 4. Build HVarPart core examples -----
#----------------------------------------------------------#

vec_selected_ids <-
  data_selected_examples[["dataset_id"]]
list_raw_by_dataset <-
  split(data_core_raw, data_core_raw[["dataset_id"]])
list_observed_by_dataset <-
  split(data_core_observed, data_core_observed[["dataset_id"]])
list_predictions_by_dataset <-
  split(data_core_predictions, data_core_predictions[["dataset_id"]])
list_metadata_by_dataset <-
  split(data_core_metadata, data_core_metadata[["dataset_id"]])

data_figure_inputs <-
  data_selected_examples %>%
  dplyr::transmute(
    dataset_id = .data[["dataset_id"]],
    example_type = .data[["example_type"]],
    data_raw = unname(list_raw_by_dataset[vec_selected_ids]),
    data_observed = unname(list_observed_by_dataset[vec_selected_ids]),
    data_predictions = unname(
      list_predictions_by_dataset[vec_selected_ids]
    ),
    data_metadata = unname(list_metadata_by_dataset[vec_selected_ids])
  ) %>%
  dplyr::mutate(
    plot = purrr::pmap(
      .l = list(
        data_raw,
        data_observed,
        data_predictions,
        data_metadata
      ),
      .f = ~ plot_hvarpart_core_temporal_example(
        data_raw = ..1,
        data_observed = ..2,
        data_predictions = ..3,
        data_metadata = ..4,
        data_importance = data_hvarpart_importance
      )
    ),
    panel_label = stringr::str_glue(
      "{c('A', 'B')} ",
      "{stringr::str_to_title(as.character(example_type))}-dominated example"
    ),
    plot_labelled = purrr::map2(
      .x = plot,
      .y = panel_label,
      .f = ~ cowplot::ggdraw() +
        cowplot::draw_plot(
          plot = .x,
          x = 0,
          y = 0,
          width = 1,
          height = 0.96
        ) +
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
        "HVarPart_core_temporal_examples.{.x}"
      )
    ),
    plot = plot_examples,
    width = 360,
    height = 300,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
)
