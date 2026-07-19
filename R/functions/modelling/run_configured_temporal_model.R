#' @title Run one configured temporal model
#' @description
#' Run one temporal model through interruption recovery, fitting, persistence,
#' seed advancement, and run-history recording.
#' @param model_id Character scalar model identifier.
#' @param data_source Data frame containing all temporal model input data.
#' @param config_dir Character scalar directory containing the model config.
#' @param model_dir Character scalar directory for fitted model files.
#' @param path_history Character scalar path to the run-history CSV.
#' @param data_interrupted_runs Data frame returned by
#' `get_interrupted_model_runs()` or an empty data frame.
#' @param config_file_name Character scalar model configuration basename.
#' @param git_commit Optional Git commit identifier recorded in run history.
#' @param git_is_dirty Optional logical indicating uncommitted changes.
#' @param verbose Logical. If `TRUE`, print fitting progress.
#' @return Invisibly returns the selected updated configuration row, or `NULL`
#' when the model is ineligible or does not require fitting.
#' @examples
#' dontrun{
#' run_configured_temporal_model(
#'   model_id = "pap_temporal__n0__Europe__Temperate",
#'   data_source = data_general_model,
#'   config_dir = "Data/Temporal_models",
#'   model_dir = "Data/Temporal_models/Mods",
#'   path_history = "Data/Temporal_models/general_model_run_history.csv"
#' )
#' }
run_configured_temporal_model <- function(
  model_id,
  data_source,
  config_dir,
  model_dir,
  path_history,
  data_interrupted_runs = tibble::tibble(),
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
    is.data.frame(data_source),
    msg = "`data_source` must be a data frame."
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
  assertthat::assert_that(
    is.data.frame(data_interrupted_runs),
    msg = "`data_interrupted_runs` must be a data frame."
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
    isFALSE(sel_mod_config[["is_model_eligible"]][1])
  ) {
    return(invisible(NULL))
  }

  if (
    isTRUE(sel_mod_config[["need_to_run"]][1]) &&
      nrow(data_interrupted_runs) > 0L
  ) {
    sel_interrupted_run <-
      data_interrupted_runs %>%
      dplyr::filter(
        model_id == .env$model_id,
        run_seed_attempt == sel_mod_config[["seed_attempt"]][1],
        run_seed == sel_mod_config[["sampling_seed"]][1]
      ) %>%
      dplyr::slice_tail(n = 1L)

    if (
      nrow(sel_interrupted_run) == 1L
    ) {
      data_interrupted_event <-
        create_model_run_event(
          model_config_row = sel_mod_config,
          run_id = sel_interrupted_run[["run_id"]][1],
          event = "fit_interrupted",
          event_time = Sys.time(),
          run_seed = sel_interrupted_run[["run_seed"]][1],
          run_seed_attempt =
            sel_interrupted_run[["run_seed_attempt"]][1],
          error_message =
            "A previous fit-start event had no recorded terminal event.",
          git_commit = git_commit,
          git_is_dirty = git_is_dirty
        )

      append_model_run_event(
        data_event = data_interrupted_event,
        path_history = path_history
      )

      models_config_current <-
        advance_model_seed(
          data_config = models_config_current,
          model_ids = model_id,
          reason = "interrupted_fit_detected"
        )

      RUtilpol::save_latest_file(
        object_to_save = models_config_current,
        file_name = config_file_name,
        dir = config_dir,
        prefered_format = "csv"
      )

      sel_mod_config <-
        models_config_current %>%
        dplyr::filter(model_id == .env$model_id)
    }
  }

  configured_model_file <-
    if (
      "model_file_name" %in% names(sel_mod_config)
    ) {
      sel_mod_config[["model_file_name"]][1]
    } else {
      NA_character_
    }
  sel_mod_file_exists <-
    if (
      !is.na(configured_model_file) && nzchar(configured_model_file)
    ) {
      file.exists(
        file.path(model_dir, configured_model_file)
      )
    } else {
      RUtilpol::get_latest_file_name(
        file_name = model_id,
        dir = model_dir
      ) %>%
        is.na() %>%
        isFALSE()
    }

  if (
    isFALSE(sel_mod_config[["need_to_run"]][1]) &&
      !(
        isTRUE(sel_mod_config[["need_to_be_evaluated"]][1]) &&
          isFALSE(sel_mod_file_exists)
      )
  ) {
    return(invisible(NULL))
  }

  if (
    isTRUE(verbose)
  ) {
    cli::cli_inform(
      stringr::str_c("Will fit general model ", model_id)
    )
  }

  time_mod_start <- Sys.time()

  run_id <-
    stringr::str_c(
      model_id,
      "attempt",
      sel_mod_config[["seed_attempt"]][1],
      "seed",
      sel_mod_config[["sampling_seed"]][1],
      format(
        time_mod_start,
        format = "%Y%m%dT%H%M%S",
        tz = "UTC"
      ),
      sep = "__"
    )

  data_run_started <-
    create_model_run_event(
      model_config_row = sel_mod_config,
      run_id = run_id,
      event = "fit_started",
      event_time = time_mod_start,
      git_commit = git_commit,
      git_is_dirty = git_is_dirty
    )

  append_model_run_event(
    data_event = data_run_started,
    path_history = path_history
  )

  models_config_started <-
    RUtilpol::get_latest_file(
      file_name = config_file_name,
      dir = config_dir,
      verbose = FALSE
    ) %>%
    update_model_run_state(
      model_id = model_id,
      run_id = run_id,
      run_seed_attempt = sel_mod_config[["seed_attempt"]][1],
      run_seed = sel_mod_config[["sampling_seed"]][1],
      event = "fit_started",
      run_start_time = time_mod_start,
      event_time = time_mod_start
    )

  RUtilpol::save_latest_file(
    object_to_save = models_config_started,
    file_name = config_file_name,
    dir = config_dir,
    prefered_format = "csv"
  )

  mod <-
    fit_brms_model(
      data_source = data_source,
      model_config_row = sel_mod_config,
      verbose = verbose
    )

  time_mod_end <- Sys.time()
  fit_succeeded <-
    !all(is.na(mod))
  fit_error <-
    attr(mod, "fit_error", exact = TRUE)

  if (
    is.null(fit_error)
  ) {
    fit_error <- NA_character_
  }

  model_file_name <-
    if (
      isTRUE(fit_succeeded)
    ) {
      save_brms_model_run(
        mod = mod,
        model_dir = model_dir,
        run_id = run_id
      )
    } else {
      NA_character_
    }
  model_chain_seeds_json <-
    if (
      isTRUE(fit_succeeded)
    ) {
      get_brms_chain_seeds(mod) %>%
        as.list() %>%
        jsonlite::toJSON(
          auto_unbox = TRUE,
          digits = NA
        ) %>%
        as.character()
    } else {
      NA_character_
    }

  models_to_run_updated <-
    RUtilpol::get_latest_file(
      file_name = config_file_name,
      dir = config_dir,
      verbose = FALSE
    ) %>%
    update_model_run_state(
      model_id = model_id,
      run_id = run_id,
      run_seed_attempt = sel_mod_config[["seed_attempt"]][1],
      run_seed = sel_mod_config[["sampling_seed"]][1],
      model_file_name = model_file_name,
      model_chain_seeds_json = model_chain_seeds_json,
      event = ifelse(
        isTRUE(fit_succeeded),
        "fit_succeeded",
        "fit_failed"
      ),
      run_start_time = time_mod_start,
      event_time = time_mod_end
    )

  RUtilpol::save_latest_file(
    object_to_save = models_to_run_updated,
    file_name = config_file_name,
    dir = config_dir,
    prefered_format = "csv"
  )

  res_config <-
    models_to_run_updated %>%
    dplyr::filter(model_id == .env$model_id)

  data_run_finished <-
    create_model_run_event(
      model_config_row = res_config,
      run_id = run_id,
      event = ifelse(
        isTRUE(fit_succeeded),
        "fit_succeeded",
        "fit_failed"
      ),
      event_time = time_mod_end,
      run_seed = sel_mod_config[["sampling_seed"]][1],
      run_seed_attempt = sel_mod_config[["seed_attempt"]][1],
      model_file_name = model_file_name,
      error_message = fit_error,
      git_commit = git_commit,
      git_is_dirty = git_is_dirty
    )

  append_model_run_event(
    data_event = data_run_finished,
    path_history = path_history
  )

  return(invisible(res_config))
}
