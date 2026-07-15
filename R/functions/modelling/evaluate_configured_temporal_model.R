#' @title Evaluate one configured temporal model
#' @description
#' Evaluate one fitted temporal model, persist its diagnostics and lifecycle
#' state, advance its seed after sampler failure, and append a run-history
#' event.
#' @param model_id Character scalar model identifier.
#' @param config_dir Character scalar directory containing the model config.
#' @param model_dir Character scalar directory containing fitted model files.
#' @param path_history Character scalar path to the run-history CSV.
#' @param pareto_k_threshold Numeric Pareto-k threshold.
#' @param loo_threshold Numeric maximum proportion above the Pareto-k threshold.
#' @param rhat_threshold Numeric Rhat threshold.
#' @param rhat_threshold_quantile Numeric Rhat quantile to compare.
#' @param config_file_name Character scalar model configuration basename.
#' @param git_commit Optional Git commit identifier recorded in run history.
#' @param git_is_dirty Optional logical indicating uncommitted changes.
#' @param verbose Logical. If `TRUE`, print evaluation progress.
#' @return Invisibly returns the selected updated configuration row, or `NULL`
#' when the model is ineligible or does not require evaluation.
#' @examples
#' \dontrun{
#' evaluate_configured_temporal_model(
#'   model_id = "pap_temporal__n0__Europe__Temperate",
#'   config_dir = "Data/Temporal_models",
#'   model_dir = "Data/Temporal_models/Mods",
#'   path_history = "Data/Temporal_models/general_model_run_history.csv"
#' )
#' }
evaluate_configured_temporal_model <- function(
  model_id,
  config_dir,
  model_dir,
  path_history,
  pareto_k_threshold = 0.7,
  loo_threshold = 0.1,
  rhat_threshold = 1.1,
  rhat_threshold_quantile = 0.9,
  config_file_name = "general_model_config_table",
  git_commit = NA_character_,
  git_is_dirty = NA,
  verbose = TRUE
) {
  assertthat::assert_that(
    is.character(model_id),
    length(model_id) == 1L,
    !is.na(model_id),
    nzchar(model_id),
    msg = "`model_id` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    is.character(config_dir),
    length(config_dir) == 1L,
    dir.exists(config_dir),
    msg = "`config_dir` must be an existing directory."
  )
  assertthat::assert_that(
    is.character(model_dir),
    length(model_dir) == 1L,
    dir.exists(model_dir),
    msg = "`model_dir` must be an existing directory."
  )
  assertthat::assert_that(
    is.character(path_history),
    length(path_history) == 1L,
    !is.na(path_history),
    nzchar(path_history),
    msg = "`path_history` must be a non-empty character scalar."
  )

  diagnostic_thresholds <-
    c(
      pareto_k_threshold,
      loo_threshold,
      rhat_threshold,
      rhat_threshold_quantile
    )

  assertthat::assert_that(
    is.numeric(diagnostic_thresholds),
    all(is.finite(diagnostic_thresholds)),
    all(diagnostic_thresholds > 0),
    msg = "Diagnostic thresholds must be positive finite numbers."
  )
  assertthat::assert_that(
    is.character(config_file_name),
    length(config_file_name) == 1L,
    !is.na(config_file_name),
    nzchar(config_file_name),
    msg = "`config_file_name` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    is.logical(verbose),
    length(verbose) == 1L,
    !is.na(verbose),
    msg = "`verbose` must be one non-missing logical value."
  )

  config_dir <-
    normalizePath(
      config_dir,
      winslash = "/",
      mustWork = TRUE
    )
  model_dir <-
    normalizePath(
      model_dir,
      winslash = "/",
      mustWork = TRUE
    )

  models_config_current <-
    RUtilpol::get_latest_file(
      file_name = config_file_name,
      dir = config_dir,
      verbose = FALSE
    )

  sel_mod_config <-
    models_config_current %>%
    dplyr::filter(model_id == .env$model_id)

  assertthat::assert_that(
    nrow(sel_mod_config) == 1L,
    msg = "`model_id` must identify exactly one model configuration row."
  )

  if (
    isFALSE(sel_mod_config[["is_model_eligible"]][1]) ||
      isFALSE(sel_mod_config[["need_to_be_evaluated"]][1])
  ) {
    return(invisible(NULL))
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      stringr::str_c("Evaluating general model ", model_id)
    )
  }

  mod <-
    RUtilpol::get_latest_file(
      file_name = model_id,
      dir = model_dir,
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
    isTRUE(model_diagnostics[["need_to_run"]][1])
  evaluation_failed <-
    sampler_diagnostics_failed ||
      isFALSE(model_diagnostics[["last_run_loo_test_pass"]][1])

  models_config_updated <-
    models_config_current %>%
    dplyr::mutate(
      last_run_rhat_test_pass = dplyr::case_when(
        .default = last_run_rhat_test_pass,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_rhat_test_pass"]][1]
      ),
      last_run_rhat_test_value = dplyr::case_when(
        .default = last_run_rhat_test_value,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_rhat_test_value"]][1]
      ),
      last_run_rhat_q90 = dplyr::case_when(
        .default = last_run_rhat_q90,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_rhat_q90"]][1]
      ),
      last_run_rhat_max = dplyr::case_when(
        .default = last_run_rhat_max,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_rhat_max"]][1]
      ),
      last_run_neff_ratio_min = dplyr::case_when(
        .default = last_run_neff_ratio_min,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_neff_ratio_min"]][1]
      ),
      last_run_divergent_transitions = dplyr::case_when(
        .default = last_run_divergent_transitions,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_divergent_transitions"]][1]
      ),
      last_run_max_treedepth_transitions = dplyr::case_when(
        .default = last_run_max_treedepth_transitions,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_max_treedepth_transitions"]][1]
      ),
      last_run_loo_test_pass = dplyr::case_when(
        .default = last_run_loo_test_pass,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_loo_test_pass"]][1]
      ),
      last_run_loo_test_value = dplyr::case_when(
        .default = last_run_loo_test_value,
        model_id == .env$model_id ~
          model_diagnostics[["last_run_loo_test_value"]][1]
      ),
      need_to_be_evaluated = dplyr::case_when(
        .default = need_to_be_evaluated,
        model_id == .env$model_id ~ FALSE
      ),
      need_to_run = dplyr::case_when(
        .default = need_to_run,
        model_id == .env$model_id ~ .env$sampler_diagnostics_failed
      ),
      prediction_written = dplyr::case_when(
        .default = prediction_written,
        model_id == .env$model_id ~ FALSE
      ),
      last_prediction_date = dplyr::case_when(
        .default = as.character(last_prediction_date),
        model_id == .env$model_id ~ NA_character_
      ),
      last_evaluation_date = dplyr::case_when(
        .default = as.character(last_evaluation_date),
        model_id == .env$model_id ~ as.character(Sys.Date())
      )
    )

  if (
    isTRUE(sampler_diagnostics_failed)
  ) {
    models_config_updated <-
      advance_model_seed(
        data_config = models_config_updated,
        model_ids = model_id,
        reason = "sampler_diagnostics_failed"
      )
  }

  RUtilpol::save_latest_file(
    object_to_save = models_config_updated,
    file_name = config_file_name,
    dir = config_dir,
    prefered_format = "csv",
    verbose = verbose
  )

  sel_mod_config_updated <-
    models_config_updated %>%
    dplyr::filter(model_id == .env$model_id)

  evaluation_run_id <-
    sel_mod_config[["last_run_id"]][1]

  if (
    is.na(evaluation_run_id) || !nzchar(evaluation_run_id)
  ) {
    evaluation_run_id <-
      stringr::str_c(
        model_id,
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
        isTRUE(evaluation_failed),
        "evaluation_failed",
        "evaluation_passed"
      ),
      event_time = Sys.time(),
      run_seed = evaluation_run_seed,
      run_seed_attempt = evaluation_seed_attempt,
      model_file_name = RUtilpol::get_latest_file_name(
        file_name = model_id,
        dir = model_dir
      ),
      git_commit = git_commit,
      git_is_dirty = git_is_dirty
    )

  append_model_run_event(
    data_event = data_evaluation_event,
    path_history = path_history
  )

  return(invisible(sel_mod_config_updated))
}
