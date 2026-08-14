#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                 Validate fitted trajectories
#
#                       O. Mottl
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

sel_model_id <-
  "predictor_temporal__spd__Europe__Temperate_Without_dry_season"

max_prediction_draws <- 1000L

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")

path_model_dir <-
  file.path(path_temporal_models, "Mods")

path_figure_dir <-
  here::here(
    "Outputs",
    "Figures",
    "Diagnostics",
    "Temporal_models"
  )

path_table_dir <-
  here::here("Outputs", "Tables")

run_directory_setup(path_figure_dir)

run_directory_setup(path_table_dir)

output_stem <-
  "spd_europe_temperate_without_dry_season_prediction_validation"


#----------------------------------------------------------#
# 1. Load the configured model and fitting data -----
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
model_config_row <-
  data_model_config %>%
  dplyr::filter(model_id == sel_model_id)

assertthat::assert_that(
  nrow(model_config_row) == 1L,
  model_config_row[["is_model_eligible"]][1],
  isFALSE(model_config_row[["need_to_run"]][1]),
  isFALSE(model_config_row[["need_to_be_evaluated"]][1]),
  msg = "The selected model must be eligible, fitted, and evaluated."
)

mod_selected <-
  load_brms_model_file(
    model_dir = path_model_dir,
    model_file_name = model_config_row[["model_file_name"]][1],
    model_id = sel_model_id
  )

data_observed <-
  data_general_model %>%
  dplyr::filter(
    analysis == model_config_row[["analysis"]][1],
    variable == model_config_row[["variable"]][1],
    region == model_config_row[["region"]][1],
    climatezone == model_config_row[["climatezone"]][1],
    dataset_id %in% mod_selected$data[["dataset_id"]]
  ) %>%
  dplyr::mutate(
    dataset_id = factor(
      as.character(dataset_id),
      levels = levels(mod_selected$data[["dataset_id"]])
    )
  )

assertthat::assert_that(
  nrow(data_observed) == nrow(mod_selected$data),
  dplyr::n_distinct(data_observed[["dataset_id"]]) ==
    dplyr::n_distinct(mod_selected$data[["dataset_id"]]),
  msg = "Stored fitting data do not match the configured fitted model."
)


#----------------------------------------------------------#
# 2. Predict each dataset over its observed time span -----
#----------------------------------------------------------#

data_dataset_new <-
  prepare_model_prediction_data(
    data_source = data_observed,
    model_config_row = model_config_row,
    prediction_range = "group_observed"
  ) %>%
  dplyr::mutate(
    dataset_id = factor(
      as.character(dataset_id),
      levels = levels(mod_selected$data[["dataset_id"]])
    )
  )

n_available_draws <-
  posterior::ndraws(mod_selected)

n_prediction_draws <-
  min(max_prediction_draws, n_available_draws)

vec_prediction_draw_ids <-
  seq(
    from = 1,
    to = n_available_draws,
    length.out = n_prediction_draws
  ) %>%
  round() %>%
  unique()

mat_dataset_expected_response <-
  brms::posterior_epred(
    object = mod_selected,
    newdata = data_dataset_new,
    re_formula = NULL,
    allow_new_levels = FALSE,
    draw_ids = vec_prediction_draw_ids
  )

data_dataset_predictions <-
  summarise_prediction_draws(
    mat_draws = mat_dataset_expected_response,
    data_new = data_dataset_new,
    group_var = NULL
  ) %>%
  dplyr::mutate(
    analysis = model_config_row[["analysis"]][1],
    model_id = sel_model_id,
    variable = model_config_row[["variable"]][1],
    source_model_file = model_config_row[["model_file_name"]][1],
    prediction_draws_used = length(vec_prediction_draw_ids),
    prediction_range = "group_observed"
  )


#----------------------------------------------------------#
# 3. Reproduce the general trend -----
#----------------------------------------------------------#

data_supported_general <-
  summarise_prediction_draws(
    mat_draws = mat_dataset_expected_response,
    data_new = data_dataset_new,
    group_var = model_config_row[["group_var"]][1]
  ) %>%
  dplyr::mutate(
    analysis = model_config_row[["analysis"]][1],
    model_id = sel_model_id,
    variable = model_config_row[["variable"]][1],
    source_model_file = model_config_row[["model_file_name"]][1],
    prediction_draws_used = length(vec_prediction_draw_ids),
    prediction_range = "group_observed",
    trend_definition = "Observed dataset support"
  )

data_configured_new <-
  prepare_model_prediction_data(
    data_source = data_observed,
    model_config_row = model_config_row,
    prediction_range = "configured"
  ) %>%
  dplyr::mutate(
    dataset_id = factor(
      as.character(dataset_id),
      levels = levels(mod_selected$data[["dataset_id"]])
    )
  )

data_configured_general <-
  predict_brms_model(
    mod = mod_selected,
    newdata = data_configured_new,
    model_config_row = model_config_row,
    max_prediction_draws = max_prediction_draws
  ) %>%
  dplyr::mutate(
    prediction_range = "configured",
    trend_definition = "Configured full range"
  )

data_general_predictions <-
  dplyr::bind_rows(
    data_configured_general,
    data_supported_general
  )

n_fitted_datasets <-
  dplyr::n_distinct(data_observed[["dataset_id"]])

data_general_comparison <-
  data_supported_general %>%
  dplyr::filter(
    n_datasets_marginalised == n_fitted_datasets
  ) %>%
  dplyr::select(
    age,
    supported_estimate = estimate
  ) %>%
  dplyr::left_join(
    data_configured_general %>%
      dplyr::select(
        age,
        configured_estimate = estimate
      ),
    by = dplyr::join_by(age)
  ) %>%
  dplyr::mutate(
    absolute_difference = abs(
      supported_estimate - configured_estimate
    )
  )

assertthat::assert_that(
  max(data_general_comparison[["absolute_difference"]]) < 1e-10,
  msg = paste(
    "Configured and independently reproduced general trends do not match",
    "where all fitted datasets are represented."
  )
)


#----------------------------------------------------------#
# 4. Plot dataset-level fits -----
#----------------------------------------------------------#

plot_dataset_predictions <-
  ggplot2::ggplot() +
  ggplot2::facet_wrap(
    ggplot2::vars(dataset_id),
    scales = "free_y",
    ncol = 8
  ) +
  ggplot2::scale_x_reverse() +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "none",
    strip.text = ggplot2::element_text(size = 6),
    axis.text = ggplot2::element_text(size = 5),
    axis.title = ggplot2::element_text(size = 9)
  ) +
  ggplot2::labs(
    x = "Age (cal ka BP)",
    y = "SPD",
    title = "Observed and fitted dataset trajectories"
  ) +
  ggplot2::geom_ribbon(
    data = data_dataset_predictions,
    mapping = ggplot2::aes(
      x = age_ka,
      ymin = conf_low,
      ymax = conf_high
    ),
    fill = "#56B4E9",
    alpha = 0.25
  ) +
  ggplot2::geom_line(
    data = data_dataset_predictions,
    mapping = ggplot2::aes(
      x = age_ka,
      y = estimate
    ),
    color = "#0072B2",
    linewidth = 0.35
  ) +
  ggplot2::geom_point(
    data = data_observed,
    mapping = ggplot2::aes(
      x = age_ka,
      y = value
    ),
    color = "#202020",
    size = 0.55,
    alpha = 0.75
  )


#----------------------------------------------------------#
# 5. Plot the general trend -----
#----------------------------------------------------------#

plot_general_prediction <-
  ggplot2::ggplot() +
  ggplot2::scale_x_reverse() +
  ggplot2::scale_color_manual(
    values = c(
      "Configured full range" = "#0072B2",
      "Observed dataset support" = "#D55E00"
    )
  ) +
  ggplot2::scale_fill_manual(
    values = c(
      "Configured full range" = "#56B4E9",
      "Observed dataset support" = "#E69F00"
    )
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom"
  ) +
  ggplot2::labs(
    x = "Age (cal ka BP)",
    y = "SPD",
    color = NULL,
    fill = NULL,
    title = "General SPD trend across fitted datasets"
  ) +
  ggplot2::geom_line(
    data = data_observed,
    mapping = ggplot2::aes(
      x = age_ka,
      y = value,
      group = dataset_id
    ),
    color = "grey55",
    linewidth = 0.25,
    alpha = 0.2
  ) +
  ggplot2::geom_ribbon(
    data = data_general_predictions,
    mapping = ggplot2::aes(
      x = age_ka,
      ymin = conf_low,
      ymax = conf_high,
      fill = trend_definition
    ),
    alpha = 0.2,
    color = NA
  ) +
  ggplot2::geom_line(
    data = data_general_predictions,
    mapping = ggplot2::aes(
      x = age_ka,
      y = estimate,
      color = trend_definition
    ),
    linewidth = 0.8
  )


#----------------------------------------------------------#
# 6. Save validation outputs -----
#----------------------------------------------------------#

readr::write_csv(
  data_dataset_predictions,
  file.path(path_table_dir, paste0(output_stem, "_dataset_predictions.csv"))
)

readr::write_csv(
  data_general_predictions,
  file.path(path_table_dir, paste0(output_stem, "_general.csv"))
)

readr::write_csv(
  data_general_comparison,
  file.path(path_table_dir, paste0(output_stem, "_comparison.csv"))
)

purrr::walk(
  c("png", "pdf"),
  ~ ggplot2::ggsave(
    filename = file.path(
      path_figure_dir,
      paste0(output_stem, "_dataset_predictions.", .x)
    ),
    plot = plot_dataset_predictions,
    width = 420,
    height = 520,
    units = "mm",
    dpi = 300,
    limitsize = FALSE,
    bg = "white"
  )
)

purrr::walk(
  c("png", "pdf"),
  ~ ggplot2::ggsave(
    filename = file.path(
      path_figure_dir,
      paste0(output_stem, "_general.", .x)
    ),
    plot = plot_general_prediction,
    width = 180,
    height = 120,
    units = "mm",
    dpi = 300,
    bg = "white"
  )
)
