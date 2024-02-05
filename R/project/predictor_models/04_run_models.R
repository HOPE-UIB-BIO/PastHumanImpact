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
  .progress = "Fitting: progress of all models",
  .l = list(
    models_to_run$region, # ..1
    models_to_run$climatezone, # ..2
    models_to_run$variable # ..3
  ),
  .f = ~ {
    # save as obejct for easier debugging
    sel_region <- ..1
    sel_climatezone <- ..2
    sel_variable <- ..3

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

    # get the selected model
    sel_mod <-
      models_to_run %>%
      dplyr::filter(
        region == sel_region &
          climatezone == sel_climatezone &
          variable == sel_variable
      )

    # check if the model needs to be run
    if (
      sel_mod$need_to_run[[1]] == FALSE
    ) {
      return()
    }

    current_env <- environment()

    # subset the data for the selected model
    sel_data_filtered <-
      data_to_fit %>%
      dplyr::filter(
        region == sel_region &
          climatezone == sel_climatezone &
          variable == sel_variable
      )

    # get the error family
    sel_error_family <-
      sel_data_filtered %>%
      purrr::pluck("error_family", 1)

    # get the raw data
    sel_data_to_fit <-
      sel_data_filtered %>%
      purrr::chuck("data_to_fit", 1)

    # get the priors
    sel_data_prior <-
      sel_data_filtered %>%
      purrr::chuck("priors", 1)

    # get the total number of iterations
    sel_set_total_iter <-
      sel_mod %>%
      purrr::chuck("total_iterations", 1)

    # get the minimum number of iterations per chain
    sel_set_min_iter_per_chain <-
      sel_mod %>%
      purrr::chuck("min_iterations_per_chain", 1)


    # Small subroutine to decrease the number of cores used if the number
    #   of iterations per chain is too small
    if (
      (sel_set_total_iter / n_cores_available) < sel_set_min_iter_per_chain
    ) {
      n_cores_to_use <-
        sel_set_total_iter / sel_set_min_iter_per_chain
    } else {
      n_cores_to_use <-
        n_cores_available
    }

    # calculate the number of iterations per chain
    sel_iter_per_chain <-
      ceiling(sel_set_total_iter / n_cores_to_use)

    # print final settings
    message(
      paste(
        "Will fit model for",
        sel_region, "in", sel_climatezone, "for", sel_variable, "\n",
        "- Using cores:", n_cores_to_use, "\n",
        "- error family:", sel_error_family, "\n",
        "- total number of iterations:", sel_set_total_iter, "\n",
        "- number of iterations per chain:", sel_iter_per_chain, "\n"
      )
    )

    # start time
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

    # end time
    time_mod_end <- Sys.time()

    # if there is and obejct
    if (
      exists("mod", envir = current_env)
    ) {
      models_to_run_updated <-
        models_to_run %>%
        dplyr::mutate(
          last_run_date = dplyr::case_when(
            .default = as.character(last_run_date),
            region == sel_region &
              climatezone == sel_climatezone &
              variable == sel_variable ~ as.character(Sys.Date())
          ),
          last_run_start_time = dplyr::case_when(
            .default = as.character(last_run_start_time),
            region == sel_region &
              climatezone == sel_climatezone &
              variable == sel_variable ~ as.character(time_mod_start)
          ),
          last_run_end_time = dplyr::case_when(
            .default = as.character(last_run_end_time),
            region == sel_region &
              climatezone == sel_climatezone &
              variable == sel_variable ~ as.character(time_mod_end)
          ),
          last_run_time = dplyr::case_when(
            .default = as.character(last_run_time),
            region == sel_region &
              climatezone == sel_climatezone &
              variable == sel_variable ~ as.character(
              time_mod_end - time_mod_start
            )
          ),
          need_to_be_evaluated = dplyr::case_when(
            .default = need_to_be_evaluated,
            region == sel_region &
              climatezone == sel_climatezone &
              variable == sel_variable ~ TRUE
          ),
          need_to_run = dplyr::case_when(
            .default = need_to_run,
            region == sel_region &
              climatezone == sel_climatezone &
              variable == sel_variable ~ FALSE
          )
        )

      # save the updated table
      RUtilpol::save_latest_file(
        object_to_save = models_to_run_updated,
        file_name = "predictor_models_config_table",
        dir = paste0(
          data_storage_path,
          "Data/Predictor_models/"
        ),
        prefered_format = "csv"
      )

      # save the model
      RUtilpol::save_latest_file(
        object_to_save = mod,
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
        prefered_format = "qs",
        preset = "archive"
      )
    }
  }
)
