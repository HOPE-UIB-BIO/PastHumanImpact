#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                  Extract general trends
#
#                   O. Mottl, V.A. Felde
#                         2023
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

rewrite <- FALSE


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

models_config_table <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

# select models to predict

models_to_predict <-
  models_config_table %>%
  dplyr::filter(
    need_to_run == FALSE &
      need_to_be_evaluated == FALSE
  ) %>%
  dplyr::mutate(
    is_h2_predictor = dplyr::case_when(
      .default = FALSE,
      variable == "temp_annual" ~ TRUE,
      variable == "temp_cold" ~ TRUE,
      variable == "prec_summer" ~ TRUE,
      variable == "prec_win" ~ TRUE,
      variable == "spd" ~ TRUE
    )
  )

# predict general trends

purrr::pwalk(
  .progress = "Prediction: progress of all models",
  .l = list(
    models_to_predict$region, # ..1
    models_to_predict$climatezone, # ..2
    models_to_predict$variable # ..3
  ),
  .f = ~ {
    sel_region <- ..1
    sel_climatezone <- ..2
    sel_variable <- ..3

    sel_file_name <-
      paste(
        sel_variable,
        sel_region,
        sel_climatezone,
        sep = "__"
      )

    predictor_models_dir <-
      paste0(
        data_storage_path,
        "Data/Predictor_models/"
      )

    general_trends_dir <-
      paste0(
        predictor_models_dir,
        "General_trends"
      )

    file_exists <-
      RUtilpol::get_latest_file_name(
        file_name = sel_file_name,
        dir = general_trends_dir
      ) %>%
      is.na() %>%
      isFALSE()

    if (
      isTRUE(file_exists) && isFALSE(rewrite)
    ) {
      return()
    }

    message(
      paste(
        "Predicting general trend for",
        sel_file_name
      )
    )

    # load the model -----
    mod <-
      RUtilpol::get_latest_file(
        file_name = sel_file_name,
        dir = paste0(
          predictor_models_dir,
          "Mods"
        ),
        verbose = FALSE
      )

    # predict general trend -----
    data_predicted <-
      predict_brms_model(mod) %>%
      dplyr::select(-group) %>%
      round(., digits = 8) %>%
      tibble::as_tibble() %>%
      janitor::clean_names() %>%
      dplyr::mutate(
        region = sel_region,
        climatezone = sel_climatezone,
        variable = sel_variable
      )

    # save results -----
    RUtilpol::save_latest_file(
      object_to_save = data_predicted,
      file_name = sel_file_name,
      dir = general_trends_dir,
      prefered_format = "qs",
      preset = "high"
    )
  }
)
