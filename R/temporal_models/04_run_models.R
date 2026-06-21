#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                         Run models
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

make_dir(
  paste0(
    data_storage_path,
    "Temporal_models/Mods"
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

models_to_run_order <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  ) %>%
  dplyr::arrange(analysis, variable)


#----------------------------------------------------------#
# 2. Run models -----
#----------------------------------------------------------#

purrr::walk(
  .progress = "Fitting general temporal models",
  .x = models_to_run_order[["model_id"]],
  .f = ~ {
    sel_model_id <- .x

    models_config_current <-
      RUtilpol::get_latest_file(
        file_name = "general_model_config_table",
        dir = paste0(
          data_storage_path,
          "Temporal_models/"
        ),
        verbose = FALSE
      )

    sel_mod_config <-
      models_config_current %>%
      dplyr::filter(model_id == sel_model_id)

    sel_mod_file_exists <-
      RUtilpol::get_latest_file_name(
        file_name = sel_model_id,
        dir = paste0(
          data_storage_path,
          "Temporal_models/Mods"
        )
      ) %>%
      is.na() %>%
      isFALSE()

    if (
      isFALSE(sel_mod_config[["need_to_run"]][1]) &&
        !(
          isTRUE(sel_mod_config[["need_to_be_evaluated"]][1]) &&
            isFALSE(sel_mod_file_exists)
        )
    ) {
      return()
    }

    message(
      paste(
        "Will fit general model",
        sel_model_id
      )
    )

    time_mod_start <- Sys.time()

    mod <-
      fit_brms_model(
        data_source = data_general_model,
        model_config_row = sel_mod_config,
        verbose = TRUE
      )

    time_mod_end <- Sys.time()

    if (
      !all(is.na(mod))
    ) {
      RUtilpol::save_latest_file(
        object_to_save = mod,
        file_name = sel_model_id,
        dir = paste0(
          data_storage_path,
          "Temporal_models/Mods"
        ),
        prefered_format = "qs"
      )
    }

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
        last_run_date = dplyr::case_when(
          .default = as.character(last_run_date),
          model_id == sel_model_id ~ as.character(Sys.Date())
        ),
        last_run_start_time = dplyr::case_when(
          .default = as.character(last_run_start_time),
          model_id == sel_model_id ~ as.character(time_mod_start)
        ),
        last_run_end_time = dplyr::case_when(
          .default = as.character(last_run_end_time),
          model_id == sel_model_id ~ as.character(time_mod_end)
        ),
        last_run_time = dplyr::case_when(
          .default = as.character(last_run_time),
          model_id == sel_model_id ~ paste(
            as.character(
              round(time_mod_end - time_mod_start, 2)
            ),
            units(time_mod_end - time_mod_start)
          )
        ),
        need_to_be_evaluated = dplyr::case_when(
          .default = need_to_be_evaluated,
          model_id == sel_model_id ~ !all(is.na(mod))
        ),
        need_to_run = dplyr::case_when(
          .default = need_to_run,
          model_id == sel_model_id ~ all(is.na(mod))
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

  }
)
