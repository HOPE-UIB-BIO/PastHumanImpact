#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                         Run models
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

library(brms)
library(parallelly)

n_cores_available <-
  as.numeric(parallelly::availableCores())


#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_data_to_fit",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

models_to_run <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  ) %>%
  # start with the smallest model first
  dplyr::arrange(n_records)


#----------------------------------------------------------#
# 1. Run models -----
#----------------------------------------------------------#

purrr::pwalk(
  .progress = "progress of all models",
  .l = list(
    region, # ..1
    climatezone, # ..2
    variable # ..3
  ),
  .f = {
    # update the table
    models_to_run <-
      RUtilpol::get_latest_file(
        file_name = "predictor_models_config_table",
        dir = paste0(
          data_storage_path,
          "Data/Predictor_models/"
        )
      ) %>%
      # start with the smallest model first
      dplyr::arrange(n_records)

    sel_mod <-
      dplyr::filter(
        region == ..1 &
          climatezone == ..2 &
          variable == ..3
      )

    if (
      sel_mod$need_to_run[[1]] == FALSE
    ) {
      return()
    }

    current_env <- environment()

    sel_data_filtered <-
      data_to_fit %>%
      dplyr::filter(
        region == ..1 &
          climatezone == ..2 &
          variable == ..3
      )

    sel_error_family <-
      sel_data_filtered %>%
      purrr::pluck("error_family", 1)

    sel_data_to_fit <-
      sel_data_filtered %>%
      purrr::chuck("data_to_fit", 1)

    sel_data_prior <-
      sel_data_filtered %>%
      purrr::chuck("priors", 1)

    sel_set_total_iter <-
      sel_mod$total_iterations[[1]]

    sel_set_min_iter_per_chain <-
      sel_mod$min_iterations_per_chain[[1]]

    if (
      (sel_set_total_iter / n_cores_available) < sel_set_min_iter_per_chain
    ) {
      n_cores_to_use <-
        sel_set_total_iter / sel_set_min_iter_per_chain
    } else {
      n_cores_to_use <-
        n_cores_available
    }

    sel_iter_per_chain <-
      ceiling(sel_set_total_iter / n_cores_to_use)

    time_mod_start <- Sys.time()

    # Run model
    mod <-
      fit_brms_hgam(
        y_var = "value",
        data_source = sel_data_to_fit,
        error_family = sel_error_family,
        chains = n_cores_to_use,
        iter = sel_iter_per_chain,
        prior = sel_data_prior,
        control = list(adapt_delta = 0.9)
      )

    time_mod_end <- Sys.time()

    if (
      exists("mod", envir = current_env)
    ) {
      models_to_run_updated <-
        models_to_run %>%
        dplyr::mutate(
          last_run_date = dplyr::case_when(
            .default = last_run_date,
            region == ..1 &
              climatezone == ..2 &
              variable == ..3 ~ as.character(Sys.Date())
          ),
          last_run_start_time = dplyr::case_when(
            .default = last_run_start_time,
            region == ..1 &
              climatezone == ..2 &
              variable == ..3 ~ as.character(time_mod_start)
          ),
          last_run_end_time = dplyr::case_when(
            .default = last_run_end_time,
            region == ..1 &
              climatezone == ..2 &
              variable == ..3 ~ as.character(time_mod_end)
          ),
          last_run_time = dplyr::case_when(
            .default = last_run_time,
            region == ..1 &
              climatezone == ..2 &
              variable == ..3 ~ as.character(time_mod_end - time_mod_start)
          ),
          need_to_be_evaluated = dplyr::case_when(
            .default = need_to_be_evaluated,
            region == ..1 &
              climatezone == ..2 &
              variable == ..3 ~ TRUE
          ),
          need_to_run = dplyr::case_when(
            .default = need_to_run,
            region == ..1 &
              climatezone == ..2 &
              variable == ..3 ~ FALSE
          )
        )

      RUtilpol::save_latest_file(
        object_to_save = models_to_run_updated,
        file_name = "predictor_models_config_table",
        dir = paste0(
          data_storage_path,
          "Data/Predictor_models/"
        ),
        prefered_format = "csv"
      )

      RUtilpol::save_latest_file(
        object_to_save = sel_mod,
        file_name = paste(
          ..3,
          ..1,
          ..2,
          sep = "__"
        ),
        dir = paste0(
          data_storage_path,
          "Data/Predictor_models/Mods"
        ),
        prefered_format = "qs",
        preset = "archive"
      )
    }
  }
)
