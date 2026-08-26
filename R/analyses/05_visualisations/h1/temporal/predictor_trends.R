#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                    Predictor trends
#                Predictor temporal trends
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
  here::here("R/00_Config_file.R")
)

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")
path_figure_dir <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Temporal",
    "Predictor_trends"
  )

run_directory_setup(path_figure_dir)


#----------------------------------------------------------#
# 1. Load and validate data -----
#----------------------------------------------------------#

data_predictor_observed <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = path_temporal_models
  ) %>%
  dplyr::filter(
    analysis == "predictor_temporal",
    region != "Africa"
  ) %>%
  prepare_climatezone_factor() %>%
  prepare_region_factor() %>%
  dplyr::select(
    region,
    climatezone,
    dataset_id,
    age,
    variable,
    value
  )

path_model_config <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  )

data_predictor_predictions <-
  predict_general_trends(
    data_source = path_model_config,
    sel_type = "predictors"
  ) %>%
  prepare_climatezone_factor() %>%
  prepare_region_factor() %>%
  dplyr::filter(
    region != "Africa",
    age <= 8500,
    !(variable == "spd" & age < 2000)
  )

data_figure_specs <-
  tibble::tibble(
    variable = c(
      "spd",
      "temp_annual",
      "temp_cold",
      "prec_summer",
      "prec_win"
    ),
    y_limits = list(
      c(0, 2.5),
      c(-10, 20),
      c(-30, 25),
      c(0, 1000),
      c(0, 2000)
    )
  )

assertthat::assert_that(
  all(data_figure_specs[["variable"]] %in%
    data_predictor_observed[["variable"]]),
  all(data_figure_specs[["variable"]] %in%
    data_predictor_predictions[["variable"]]),
  msg = "All supplementary predictors require observations and predictions."
)


#----------------------------------------------------------#
# 2. Build and save supplementary figures -----
#----------------------------------------------------------#

data_figures <-
  data_figure_specs %>%
  dplyr::mutate(
    plot = purrr::map2(
      .x = variable,
      .y = y_limits,
      .f = ~ plot_predictor_temporal_trends(
        data_observed = data_predictor_observed,
        data_predictions = data_predictor_predictions,
        variable = .x,
        y_limits = .y
      )
    )
  ) %>%
  tidyr::expand_grid(extension = c("png", "pdf"))

data_figures %>%
  purrr::pwalk(
    .f = ~ ggplot2::ggsave(
      filename = file.path(
        path_figure_dir,
        stringr::str_glue(
          "predictor__temporal_trend__{..1}.{..4}"
        )
      ),
      plot = ..3,
      width = image_width_vec[["3col"]],
      height = 200,
      units = image_units,
      dpi = 300,
      bg = "white"
    )
  )
