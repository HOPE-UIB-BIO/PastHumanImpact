#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                     Predictor models
#                      Evaluate models
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


test_for_predictive_power <- FALSE

pareto_k_threshold <- 0.7
loo_threshold <- 0.1
rhat_threshold <- 1.1
rhat_threshold_quantile <- 0.9



#----------------------------------------------------------#
# 0. Load data -----
#----------------------------------------------------------#

models_to_run <-
  RUtilpol::get_latest_file(
    file_name = "predictor_models_config_table",
    dir = paste0(
      data_storage_path,
      "Data/Predictor_models/"
    )
  )

#----------------------------------------------------------#
# 1. Evaluate models -----
#----------------------------------------------------------#

purrr::pwalk(
  .progress = "Evaluation: progress of all models",
  .l = list(
    models_to_run$region, # ..1
    models_to_run$climatezone, # ..2
    models_to_run$variable # ..3
  ),
  .f = ~ {
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
        ),
        verbose = FALSE
      )

    sel_mod <-
      models_to_run %>%
      dplyr::filter(
        region == sel_region &
          climatezone == sel_climatezone &
          variable == sel_variable
      )

    if (
      isFALSE(sel_mod$need_to_be_evaluated[1])
    ) {
      return()
    }

    message(
      paste(
        "Evaluating model for",
        sel_variable,
        "in",
        sel_region,
        "and",
        sel_climatezone
      )
    )

    # load the model -----
    mod <-
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
        verbose = FALSE
      )

    # does the model exist?
    if (
      all(is.na(mod))
    ) {
      flag_model_to_rerun(
        models_to_run, sel_region, sel_climatezone, sel_variable
      )

      return()
    }

    # contain draws? -----

    if (
      length(mod$fit@sim) == 0
    ) {
      flag_model_to_rerun(
        models_to_run, sel_region, sel_climatezone, sel_variable
      )

      return()
    }

    # LOO test -----

    loo_res <-
      brms::loo(mod)

    loo_length <-
      length(loo_res$diagnostics$pareto_k)

    loo_above_threshold <-
      (loo_res$diagnostics$pareto_k >= pareto_k_threshold)

    loo_value <-
      (sum(loo_above_threshold) / loo_length)

    pass_loo_test <-
      loo_value <= loo_threshold

    # rhat test -----

    rhat_res <-
      brms::rhat(mod)

    avg_rhat_value <-
      mean(rhat_res, na.mr = TRUE)

    # We consider as pass if 90% of the values are below the threshold
    pass_rhat_test <-
      as.logical(
        stats::quantile(rhat_res, rhat_threshold_quantile) <= rhat_threshold
      )

    # evaluation summary -----

    is_event <-
      dplyr::case_when(
        .default = FALSE,
        sel_variable == "bi" ~ TRUE,
        sel_variable == "fi" ~ TRUE,
        sel_variable == "fc" ~ TRUE,
        sel_variable == "ec" ~ TRUE,
        sel_variable == "ei" ~ TRUE,
        sel_variable == "cc" ~ TRUE,
        sel_variable == "es" ~ TRUE,
        sel_variable == "weak" ~ TRUE,
        sel_variable == "medium" ~ TRUE,
        sel_variable == "strong" ~ TRUE
      )

    need_to_be_rerun <-
      isFALSE(pass_rhat_test) |
        # (isFALSE(pass_loo_test) & isFALSE(is_event)) |
        (isFALSE(pass_loo_test) & isTRUE(test_for_predictive_power))

    # Update the model table -----

    models_to_run_updated <-
      models_to_run %>%
      dplyr::mutate(
        last_run_rhat_test_pass = dplyr::case_when(
          .default = last_run_rhat_test_pass,
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ pass_rhat_test
        ),
        last_run_rhat_test_value = dplyr::case_when(
          .default = last_run_rhat_test_value,
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ avg_rhat_value
        ),
        last_run_loo_test_pass = dplyr::case_when(
          .default = last_run_loo_test_pass,
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ pass_loo_test
        ),
        last_run_loo_test_value = dplyr::case_when(
          .default = last_run_loo_test_value,
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ loo_value
        ),
        need_to_be_evaluated = dplyr::case_when(
          .default = need_to_be_evaluated,
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ FALSE
        ),
        need_to_run = dplyr::case_when(
          .default = need_to_run,
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ need_to_be_rerun
        ),
        last_evaluation_date = dplyr::case_when(
          .default = as.character(last_evaluation_date),
          region == sel_region &
            climatezone == sel_climatezone &
            variable == sel_variable ~ as.character(Sys.Date())
        )
      )

    # save the updated table -----
    RUtilpol::save_latest_file(
      object_to_save = models_to_run_updated,
      file_name = "predictor_models_config_table",
      dir = paste0(
        data_storage_path,
        "Data/Predictor_models/"
      ),
      prefered_format = "csv",
      verbose = TRUE
    )
  }
)
