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

path_model_run_history <-
  file.path(
    data_storage_path,
    "Temporal_models",
    "general_model_run_history.csv"
  )

git_commit <-
  tryCatch(
    system2(
      command = "git",
      args = c("rev-parse", "HEAD"),
      stdout = TRUE,
      stderr = FALSE
    )[1],
    error = function(err) NA_character_
  )

git_status <-
  tryCatch(
    system2(
      command = "git",
      args = c("status", "--porcelain"),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(err) NA_character_
  )

git_is_dirty <-
  if (
    all(is.na(git_status))
  ) {
    NA
  } else {
    length(git_status) > 0L
  }


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
      isFALSE(sel_mod_config[["is_model_eligible"]][1]) ||
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

    sampler_diagnostics_failed <-
      isFALSE(model_diagnostics[["last_run_rhat_test_pass"]][1]) ||
      model_diagnostics[["last_run_divergent_transitions"]][1] > 0L ||
      model_diagnostics[["last_run_max_treedepth_transitions"]][1] > 0L

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
        prediction_written = dplyr::case_when(
          .default = prediction_written,
          model_id == sel_model_id ~ FALSE
        ),
        last_prediction_date = dplyr::case_when(
          .default = as.character(last_prediction_date),
          model_id == sel_model_id ~ NA_character_
        ),
        last_evaluation_date = dplyr::case_when(
          .default = as.character(last_evaluation_date),
          model_id == sel_model_id ~ as.character(Sys.Date())
        )
      )

    if (
      isTRUE(sampler_diagnostics_failed)
    ) {
      models_to_run_updated <-
        advance_model_seed(
          data_config = models_to_run_updated,
          model_ids = sel_model_id,
          reason = "sampler_diagnostics_failed"
        )
    }

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

    sel_mod_config_updated <-
      models_to_run_updated %>%
      dplyr::filter(model_id == sel_model_id)

    evaluation_run_id <-
      sel_mod_config[["last_run_id"]][1]

    if (
      is.na(evaluation_run_id) || !nzchar(evaluation_run_id)
    ) {
      evaluation_run_id <-
        stringr::str_c(
          sel_model_id,
          "legacy_evaluation",
          as.character(Sys.Date()),
          sep = "__"
        )
    }

    evaluation_run_seed <-
      sel_mod_config[["last_run_seed"]][1]

    if (
      is.na(evaluation_run_seed)
    ) {
      evaluation_run_seed <-
        sel_mod_config[["sampling_seed"]][1]
    }

    evaluation_seed_attempt <-
      sel_mod_config[["last_run_seed_attempt"]][1]

    if (
      is.na(evaluation_seed_attempt)
    ) {
      evaluation_seed_attempt <-
        sel_mod_config[["seed_attempt"]][1]
    }

    data_evaluation_event <-
      create_model_run_event(
        model_config_row = sel_mod_config_updated,
        run_id = evaluation_run_id,
        event = ifelse(
          isTRUE(model_diagnostics[["need_to_run"]][1]),
          "evaluation_failed",
          "evaluation_passed"
        ),
        event_time = Sys.time(),
        run_seed = evaluation_run_seed,
        run_seed_attempt = evaluation_seed_attempt,
        model_file_name = RUtilpol::get_latest_file_name(
          file_name = sel_model_id,
          dir = paste0(
            data_storage_path,
            "Temporal_models/Mods"
          )
        ),
        git_commit = git_commit,
        git_is_dirty = git_is_dirty
      )

    append_model_run_event(
      data_event = data_evaluation_event,
      path_history = path_model_run_history
    )
  }
)
