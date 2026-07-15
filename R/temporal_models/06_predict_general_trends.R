#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                    Predict trajectories
#
#                   O. Mottl, V.A. Felde
#                         2024
#
#----------------------------------------------------------#


#----------------------------------------------------------#
# 0. Setup -----
#----------------------------------------------------------#

source(
  here::here(
    "R/00_Config_file.R"
  )
)

rewrite <- FALSE

make_dir(
  paste0(
    data_storage_path,
    "Temporal_models/General_trends"
  )
)


#----------------------------------------------------------#
# 1. Load data -----
#----------------------------------------------------------#

data_general_model <-
  RUtilpol::get_latest_file(
    file_name = "general_temporal_model_data",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )

models_config_table <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )


#----------------------------------------------------------#
# 2. Predict models -----
#----------------------------------------------------------#

list_predictions <-
  purrr::map(
    .progress = "Predicting general temporal models",
    .x = models_config_table[["model_id"]],
    .f = ~ {
      sel_model_id <- .x

      sel_mod_config <-
        models_config_table %>%
        dplyr::filter(model_id == sel_model_id)

      if (
        isFALSE(sel_mod_config[["is_model_eligible"]][1])
      ) {
        return(NULL)
      }

      if (
        isTRUE(sel_mod_config[["need_to_run"]][1]) ||
          isTRUE(sel_mod_config[["need_to_be_evaluated"]][1])
      ) {
        return(NULL)
      }

      sel_file_exists <-
        RUtilpol::get_latest_file_name(
          file_name = sel_model_id,
          dir = paste0(
            data_storage_path,
            "Temporal_models/General_trends"
          )
        ) %>%
        is.na() %>%
        isFALSE()

      if (
        isTRUE(sel_file_exists) &&
          isFALSE(rewrite) &&
          isTRUE(sel_mod_config[["prediction_written"]][1])
      ) {
        data_existing <-
          RUtilpol::get_latest_file(
            file_name = sel_model_id,
            dir = paste0(
              data_storage_path,
              "Temporal_models/General_trends"
            ),
            verbose = FALSE
          )

        return(data_existing)
      }

      mod <-
        load_brms_model_file(
          model_dir = file.path(
            data_storage_path,
            "Temporal_models",
            "Mods"
          ),
          model_file_name = sel_mod_config[["model_file_name"]][1],
          model_id = sel_model_id
        )

      if (
        all(is.na(mod))
      ) {
        flag_model_to_rerun(
          data_source = models_config_table,
          sel_model_id = sel_model_id,
          config_file_name = "general_model_config_table"
        )

        return(NULL)
      }

      data_new <-
        get_model_newdata(
          data_source = data_general_model,
          model_config_row = sel_mod_config
        )

      data_predicted <-
        predict_brms_model(
          mod = mod,
          newdata = data_new,
          model_config_row = sel_mod_config
        ) %>%
        dplyr::mutate(
          value = estimate,
          dplyr::across(
            dplyr::where(is.numeric),
            ~ round(.x, digits = 8)
          )
        )

      RUtilpol::save_latest_file(
        object_to_save = data_predicted,
        file_name = sel_model_id,
        dir = paste0(
          data_storage_path,
          "Temporal_models/General_trends"
        ),
        prefered_format = "csv"
      )

      models_to_run_updated <-
        RUtilpol::get_latest_file(
          file_name = "general_model_config_table",
          dir = paste0(
            data_storage_path,
            "Temporal_models/"
          ),
          verbose = FALSE
        ) %>%
        dplyr::mutate(
          prediction_written = dplyr::case_when(
            .default = prediction_written,
            model_id == sel_model_id ~ TRUE
          ),
          last_prediction_date = dplyr::case_when(
            .default = as.character(last_prediction_date),
            model_id == sel_model_id ~ as.character(Sys.Date())
          )
        )

      RUtilpol::save_latest_file(
        object_to_save = models_to_run_updated,
        file_name = "general_model_config_table",
        dir = paste0(
          data_storage_path,
          "Temporal_models/"
        ),
        prefered_format = "csv"
      )

      return(data_predicted)
    }
  )

data_predictions <-
  list_predictions %>%
  purrr::compact() %>%
  dplyr::bind_rows()

if (
  nrow(data_predictions) > 0
) {
  RUtilpol::save_latest_file(
    object_to_save = data_predictions,
    file_name = "general_temporal_model_predictions",
    dir = paste0(
      data_storage_path,
      "Temporal_models/General_trends"
    ),
    prefered_format = "csv"
  )

  data_pap_predictions <-
    data_predictions %>%
    dplyr::filter(analysis == "pap_temporal")

  if (
    nrow(data_pap_predictions) > 0
  ) {
    RUtilpol::save_latest_file(
      object_to_save = data_pap_predictions,
      file_name = "pap_temporal_model_predictions",
      dir = paste0(
        data_storage_path,
        "Temporal_models/General_trends"
      ),
      prefered_format = "csv"
    )

    readr::write_csv(
      data_pap_predictions,
      here::here(
        "Outputs/Tables/pap_temporal_trends_by_region_climatezone.csv"
      )
    )
  }
}
