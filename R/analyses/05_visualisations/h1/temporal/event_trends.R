#----------------------------------------------------------#
#
#
#                   GlobalHumanImpact
#
#                     Event trends
#                 Event temporal trends
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

path_temporal_models <-
  file.path(data_storage_path, "Temporal_models")
path_figure_dir <-
  here::here(
    "Outputs",
    "Figures",
    "H1",
    "Temporal",
    "Event_trends"
  )

run_directory_setup(path_figure_dir)


#----------------------------------------------------------#
# 1. Load and validate data -----
#----------------------------------------------------------#

data_model_config <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = path_temporal_models
  )

data_event_predictions_modelled <-
  predict_general_trends(
    data_source = data_model_config,
    sel_type = "events"
  ) %>%
  dplyr::select(
    region,
    climatezone,
    age,
    variable,
    estimate,
    conf_low,
    conf_high
  )

data_event_predictions_constant <-
  RUtilpol::get_latest_file(
    file_name = "temporal_model_data_constant",
    dir = path_temporal_models
  ) %>%
  dplyr::semi_join(
    data_model_config %>%
      dplyr::filter(
        analysis == "event_temporal",
        !is_model_eligible,
        ineligibility_reason == "constant_response"
      ) %>%
      dplyr::select(region, climatezone, variable),
    by = c("region", "climatezone", "variable")
  ) %>%
  dplyr::filter(dplyr::between(age, 500, 8500)) %>%
  dplyr::mutate(
    estimate = value,
    conf_low = value,
    conf_high = value
  ) %>%
  dplyr::select(
    region,
    climatezone,
    age,
    variable,
    estimate,
    conf_low,
    conf_high
  )

data_event_predictions <-
  dplyr::bind_rows(
    data_event_predictions_modelled,
    data_event_predictions_constant
  ) %>%
  prepare_climatezone_factor() %>%
  prepare_region_factor()

data_expected_strata <-
  data_model_config %>%
  dplyr::filter(analysis == "event_temporal") %>%
  dplyr::distinct(region, climatezone, variable)
data_prediction_strata <-
  data_event_predictions %>%
  dplyr::distinct(region, climatezone, variable)

assertthat::assert_that(
  nrow(data_prediction_strata) == nrow(data_expected_strata),
  all(data_event_predictions[["estimate"]] >= 0),
  all(data_event_predictions[["estimate"]] <= 1),
  msg = "Event predictions must cover every configured stratum on [0, 1]."
)


#----------------------------------------------------------#
# 2. Build and save supplementary figure -----
#----------------------------------------------------------#

plot_event_temporal <-
  plot_event_temporal_trends(
    data_predictions = data_event_predictions
  )

purrr::walk(
  c("png", "pdf"),
  .f = ~ ggplot2::ggsave(
    filename = file.path(
      path_figure_dir,
      stringr::str_glue("event_trends.{.x}")
    ),
    plot = plot_event_temporal,
    width = image_width_vec[["3col"]],
    height = 200,
    units = image_units,
    dpi = 300,
    bg = "white"
  )
)
