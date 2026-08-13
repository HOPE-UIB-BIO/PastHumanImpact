#' @title Build a temporal model run-history event
#' @description
#' Create one auditable event containing the seed, sampler settings, code state,
#' and a JSON snapshot of the selected model configuration.
#' @param model_config_row One-row model configuration data frame.
#' @param run_id Character scalar identifying one fit attempt.
#' @param event Character scalar event type.
#' @param event_time POSIXct scalar event time.
#' @param run_seed Optional integer seed used by the recorded fit.
#' @param run_seed_attempt Optional integer seed-attempt number.
#' @param model_file_name Optional saved model file name.
#' @param error_message Optional fitting error message.
#' @param git_commit Optional Git commit identifier.
#' @param git_is_dirty Optional logical indicating uncommitted repository
#' changes.
#' @return One-row tibble containing the run-history event.
#' @examples
#' dontrun{
#' event <- build_model_run_event(
#'   model_config_row = config[1, ],
#'   run_id = "model_a__attempt_1",
#'   event = "fit_started"
#' )
#' }
build_model_run_event <- function(
  model_config_row,
  run_id,
  event,
  event_time = Sys.time(),
  run_seed = NULL,
  run_seed_attempt = NULL,
  model_file_name = NA_character_,
  error_message = NA_character_,
  git_commit = NA_character_,
  git_is_dirty = NA
) {
  assertthat::assert_that(
    is.data.frame(model_config_row),
    nrow(model_config_row) == 1L,
    msg = "`model_config_row` must be a one-row data frame."
  )

  required_columns <-
    c(
      "model_id",
      "analysis",
      "variable",
      "region",
      "climatezone",
      "family_key",
      "model_profile",
      "formula_text",
      "total_iterations",
      "min_iterations_per_chain",
      "max_chains",
      "adapt_delta",
      "max_treedepth",
      "seed_attempt",
      "sampling_seed"
    )

  assertthat::assert_that(
    all(required_columns %in% names(model_config_row)),
    msg = "`model_config_row` is missing run-history columns."
  )
  assertthat::assert_that(
    is.character(run_id),
    length(run_id) == 1L,
    !is.na(run_id),
    nzchar(run_id),
    msg = "`run_id` must be a non-empty character scalar."
  )

  allowed_events <-
    c(
      "fit_started",
      "fit_succeeded",
      "fit_failed",
      "fit_interrupted",
      "evaluation_passed",
      "evaluation_failed"
    )

  assertthat::assert_that(
    is.character(event),
    length(event) == 1L,
    event %in% allowed_events,
    msg = "`event` is not a supported model run event."
  )
  assertthat::assert_that(
    inherits(event_time, "POSIXct"),
    length(event_time) == 1L,
    !is.na(event_time),
    msg = "`event_time` must be one non-missing POSIXct value."
  )

  if (
    is.null(run_seed)
  ) {
    run_seed <- model_config_row[["sampling_seed"]][1]
  }

  if (
    is.null(run_seed_attempt)
  ) {
    run_seed_attempt <- model_config_row[["seed_attempt"]][1]
  }

  assertthat::assert_that(
    assertthat::is.count(run_seed),
    assertthat::is.count(run_seed_attempt),
    msg = "Run seed values must be positive integers."
  )

  config_snapshot_json <-
    jsonlite::toJSON(
      as.list(model_config_row[1, , drop = FALSE]),
      auto_unbox = TRUE,
      na = "null",
      null = "null",
      digits = NA
    )

  res_event <-
    tibble::tibble(
      run_id = run_id,
      event = event,
      event_time = format(
        event_time,
        format = "%Y-%m-%dT%H:%M:%OS6Z",
        tz = "UTC"
      ),
      model_id = model_config_row[["model_id"]][1],
      analysis = model_config_row[["analysis"]][1],
      variable = model_config_row[["variable"]][1],
      region = model_config_row[["region"]][1],
      climatezone = model_config_row[["climatezone"]][1],
      run_seed_attempt = as.integer(run_seed_attempt),
      run_seed = as.integer(run_seed),
      family_key = model_config_row[["family_key"]][1],
      model_profile = model_config_row[["model_profile"]][1],
      formula_text = model_config_row[["formula_text"]][1],
      total_iterations = model_config_row[["total_iterations"]][1],
      min_iterations_per_chain =
        model_config_row[["min_iterations_per_chain"]][1],
      max_chains = model_config_row[["max_chains"]][1],
      adapt_delta = model_config_row[["adapt_delta"]][1],
      max_treedepth = model_config_row[["max_treedepth"]][1],
      model_file_name = model_file_name,
      error_message = error_message,
      git_commit = git_commit,
      git_is_dirty = as.logical(git_is_dirty),
      config_snapshot_json = as.character(config_snapshot_json)
    )

  return(res_event)
}
