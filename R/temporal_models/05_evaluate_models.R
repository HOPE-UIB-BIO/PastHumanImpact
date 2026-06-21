#----------------------------------------------------------#
#
#
#                     GlobalHumanImpact
#
#                  General temporal models
#                      Evaluate models
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

pareto_k_threshold <- 0.7
loo_threshold <- 0.1
rhat_threshold <- 1.1
rhat_threshold_quantile <- 0.9


#----------------------------------------------------------#
# 1. Load config table -----
#----------------------------------------------------------#

models_to_evaluate <-
  RUtilpol::get_latest_file(
    file_name = "general_model_config_table",
    dir = paste0(
      data_storage_path,
      "Temporal_models/"
    )
  )


#----------------------------------------------------------#
# 2. Evaluate models -----
#----------------------------------------------------------#

purrr::walk(
  .progress = "Evaluating general temporal models",
  .x = models_to_evaluate[["model_id"]],
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

    if (
      isFALSE(sel_mod_config[["need_to_be_evaluated"]][1])
    ) {
      return()
    }

    message(
      paste(
        "Evaluating general model",
        sel_model_id
      )
    )

    mod <-
      RUtilpol::get_latest_file(
        file_name = sel_model_id,
        dir = paste0(
          data_storage_path,
          "Temporal_models/Mods"
        ),
        verbose = FALSE
      )

    model_diagnostics <-
      evaluate_brms_model(
        mod = mod,
        pareto_k_threshold = pareto_k_threshold,
        loo_threshold = loo_threshold,
        rhat_threshold = rhat_threshold,
        rhat_threshold_quantile = rhat_threshold_quantile,
        max_treedepth_threshold = sel_mod_config[["max_treedepth"]][1]
      )

    models_to_run_updated <-
      models_config_current %>%
      dplyr::mutate(
        last_run_rhat_test_pass = dplyr::case_when(
          .default = last_run_rhat_test_pass,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_rhat_test_pass"]][1]
        ),
        last_run_rhat_test_value = dplyr::case_when(
          .default = last_run_rhat_test_value,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_rhat_test_value"]][1]
        ),
        last_run_rhat_q90 = dplyr::case_when(
          .default = last_run_rhat_q90,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_rhat_q90"]][1]
        ),
        last_run_rhat_max = dplyr::case_when(
          .default = last_run_rhat_max,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_rhat_max"]][1]
        ),
        last_run_neff_ratio_min = dplyr::case_when(
          .default = last_run_neff_ratio_min,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_neff_ratio_min"]][1]
        ),
        last_run_divergent_transitions = dplyr::case_when(
          .default = last_run_divergent_transitions,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_divergent_transitions"]][1]
        ),
        last_run_max_treedepth_transitions = dplyr::case_when(
          .default = last_run_max_treedepth_transitions,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_max_treedepth_transitions"]][1]
        ),
        last_run_loo_test_pass = dplyr::case_when(
          .default = last_run_loo_test_pass,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_loo_test_pass"]][1]
        ),
        last_run_loo_test_value = dplyr::case_when(
          .default = last_run_loo_test_value,
          model_id == sel_model_id ~
            model_diagnostics[["last_run_loo_test_value"]][1]
        ),
        need_to_be_evaluated = dplyr::case_when(
          .default = need_to_be_evaluated,
          model_id == sel_model_id ~ FALSE
        ),
        need_to_run = dplyr::case_when(
          .default = need_to_run,
          model_id == sel_model_id ~ model_diagnostics[["need_to_run"]][1]
        ),
        last_evaluation_date = dplyr::case_when(
          .default = as.character(last_evaluation_date),
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
      prefered_format = "csv",
      verbose = TRUE
    )
  }
)
