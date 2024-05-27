#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                 manual check of prediction
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

# Load configuration
source(
  here::here(
    "R/project/00_Config_file.R"
  )
)

sel_region <- "Europe"
sel_climatezone <- "Cold_Without_dry_season_Warm_Summer"
sel_variable <- "temp_annual"


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

# raw data
sel_raw_data <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_to_fit",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  ) %>%
  dplyr::filter(
    region == sel_region &
      climatezone == sel_climatezone &
      variable == sel_variable
  ) %>%
  purrr::chuck("data_to_fit", 1)

# load the model
sel_mod <-
  RUtilpol::get_latest_file(
    file_name = paste(
      sel_variable,
      sel_region,
      sel_climatezone,
      sep = "__"
    ),
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/Mods"
    ),
    verbose = TRUE
  )


#----------------------------------------------------------#
# 2. predict general trend -----
#----------------------------------------------------------#

data_predicted <-
  predict_brms_model(sel_mod) %>%
  dplyr::select(-group) %>%
  round(., digits = 8) %>%
  tibble::as_tibble() %>%
  janitor::clean_names()


#----------------------------------------------------------#
# 3. Visual check of predictom -----
#----------------------------------------------------------#

is_event <-
  dplyr::case_when(
    .default = TRUE,
    sel_variable == "temp_annual" ~ FALSE,
    sel_variable == "temp_cold" ~ FALSE,
    sel_variable == "prec_summer" ~ FALSE,
    sel_variable == "prec_win" ~ FALSE,
    sel_variable == "spd" ~ FALSE
  )

p0 <-
  sel_raw_data %>%
  ggplot2::ggplot(
    ggplot2::aes(
      x = age,
      y = value
    )
  ) +
  ggplot2::labs(
    title = paste(
      "Predicted general trend for",
      sel_variable,
      "in",
      sel_region,
      "and",
      sel_climatezone
    ),
    x = "Age",
    y = sel_variable
  ) +
  ggplot2::geom_point() +
  ggplot2::geom_line(
    ggplot2::aes(
      group = dataset_id
    )
  ) +
  ggplot2::geom_line(
    data = data_predicted,
    ggplot2::aes(
      x = age,
      y = value
    ),
    color = "blue"
  )

if (
  isTRUE(is_event)
) {
  p0 +
    ggplot2::facet_wrap(~dataset_id) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        group = dataset_id,
        ymin = 0,
        ymax = value
      ),
      alpha = 0.2,
      col = "grey"
    ) +
    ggplot2::geom_ribbon(
      data = data_predicted,
      ggplot2::aes(
        x = age,
        y = value,
        ymin = conf_low,
        ymax = conf_high
      ),
      alpha = 0.2,
      col = "blue"
    )
} else {
  p0 +
    ggplot2::geom_ribbon(
      data = data_predicted,
      ggplot2::aes(
        x = age,
        y = value,
        ymin = conf_low,
        ymax = conf_high
      ),
      alpha = 0.2,
      col = "grey"
    )
}
