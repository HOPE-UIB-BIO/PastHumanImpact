#' @title Resolve temporal model run state
#' @description
#' Apply a fit-start, fit-success, or fit-failure transition to one model
#' configuration row, including seed advancement after fitting failure.
#' @param data_config Model configuration data frame.
#' @param model_id Character scalar model identifier.
#' @param run_id Character scalar run identifier.
#' @param run_seed_attempt Positive integer seed-attempt number.
#' @param run_seed Positive integer sampling seed.
#' @param model_file_name Optional exact saved model filename.
#' @param model_chain_seeds_json Optional JSON object with chain-specific seeds.
#' @param event One of `fit_started`, `fit_succeeded`, or `fit_failed`.
#' @param run_start_time POSIXct scalar fit start time.
#' @param event_time POSIXct scalar time of the state transition.
#' @return Updated model configuration data frame.
#' @examples
#' dontrun{
#' config <- resolve_model_run_state(
#'   data_config = config,
#'   model_id = "model_a",
#'   run_id = "model_a__attempt_1",
#'   run_seed_attempt = 1L,
#'   run_seed = 123L,
#'   event = "fit_started",
#'   run_start_time = Sys.time(),
#'   event_time = Sys.time()
#' )
#' }
resolve_model_run_state <- function(
  data_config,
  model_id,
  run_id,
  run_seed_attempt,
  run_seed,
  model_file_name = NA_character_,
  model_chain_seeds_json = NA_character_,
  event = c("fit_started", "fit_succeeded", "fit_failed"),
  run_start_time,
  event_time
) {
  assertthat::assert_that(
    is.data.frame(data_config),
    msg = "`data_config` must be a data frame."
  )

  required_columns <-
    c(
      "model_id",
      "last_run_date",
      "last_run_id",
      "last_run_seed_attempt",
      "last_run_seed",
      "model_file_name",
      "model_chain_seeds_json",
      "model_seed_source",
      "model_provenance_status",
      "model_audit_reason",
      "last_run_start_time",
      "last_run_end_time",
      "last_run_time",
      "need_to_be_evaluated",
      "need_to_run",
      "prediction_written",
      "last_prediction_date"
    )

  assertthat::assert_that(
    all(required_columns %in% names(data_config)),
    msg = "`data_config` is missing required run-state columns."
  )
  assertthat::assert_that(
    is.character(model_id),
    length(model_id) == 1L,
    model_id %in% data_config[["model_id"]],
    msg = "`model_id` must identify a configured model."
  )
  assertthat::assert_that(
    is.character(run_id),
    length(run_id) == 1L,
    !is.na(run_id),
    nzchar(run_id),
    msg = "`run_id` must be a non-empty character scalar."
  )
  assertthat::assert_that(
    assertthat::is.count(run_seed_attempt),
    assertthat::is.count(run_seed),
    msg = "Run seed values must be positive integers."
  )
  assertthat::assert_that(
    is.character(model_file_name),
    length(model_file_name) == 1L,
    is.character(model_chain_seeds_json),
    length(model_chain_seeds_json) == 1L,
    msg = "Model provenance values must be character scalars."
  )
  assertthat::assert_that(
    inherits(run_start_time, "POSIXct"),
    length(run_start_time) == 1L,
    !is.na(run_start_time),
    inherits(event_time, "POSIXct"),
    length(event_time) == 1L,
    !is.na(event_time),
    msg = "Run state times must be non-missing POSIXct scalars."
  )

  event <- match.arg(event)
  fit_finished <- event != "fit_started"
  fit_succeeded <- event == "fit_succeeded"
  run_duration <- event_time - run_start_time

  res_config <-
    data_config %>%
    dplyr::mutate(
      last_run_date = dplyr::case_when(
        .default = as.character(last_run_date),
        model_id == .env$model_id ~ as.character(Sys.Date())
      ),
      last_run_id = dplyr::case_when(
        .default = as.character(last_run_id),
        model_id == .env$model_id ~ .env$run_id
      ),
      last_run_seed_attempt = dplyr::case_when(
        .default = as.integer(last_run_seed_attempt),
        model_id == .env$model_id ~ as.integer(.env$run_seed_attempt)
      ),
      last_run_seed = dplyr::case_when(
        .default = as.integer(last_run_seed),
        model_id == .env$model_id ~ as.integer(.env$run_seed)
      ),
      model_file_name = dplyr::case_when(
        .default = as.character(model_file_name),
        model_id == .env$model_id & .env$fit_succeeded ~
          .env$model_file_name
      ),
      model_chain_seeds_json = dplyr::case_when(
        .default = as.character(model_chain_seeds_json),
        model_id == .env$model_id & .env$fit_succeeded ~
          .env$model_chain_seeds_json
      ),
      model_seed_source = dplyr::case_when(
        .default = as.character(model_seed_source),
        model_id == .env$model_id & .env$fit_succeeded ~
          "configured_sampling_seed"
      ),
      model_provenance_status = dplyr::case_when(
        .default = as.character(model_provenance_status),
        model_id == .env$model_id & .env$fit_succeeded ~
          "configured_run_recorded"
      ),
      model_audit_reason = dplyr::case_when(
        .default = as.character(model_audit_reason),
        model_id == .env$model_id & .env$fit_succeeded ~ NA_character_
      ),
      last_run_start_time = dplyr::case_when(
        .default = as.character(last_run_start_time),
        model_id == .env$model_id ~ as.character(.env$run_start_time)
      ),
      last_run_end_time = dplyr::case_when(
        .default = as.character(last_run_end_time),
        model_id == .env$model_id & .env$fit_finished ~
          as.character(.env$event_time),
        model_id == .env$model_id ~ NA_character_
      ),
      last_run_time = dplyr::case_when(
        .default = as.character(last_run_time),
        model_id == .env$model_id & .env$fit_finished ~ paste(
          as.character(round(.env$run_duration, 2)),
          units(.env$run_duration)
        ),
        model_id == .env$model_id ~ NA_character_
      ),
      need_to_be_evaluated = dplyr::case_when(
        .default = need_to_be_evaluated,
        model_id == .env$model_id ~ .env$fit_succeeded
      ),
      need_to_run = dplyr::case_when(
        .default = need_to_run,
        model_id == .env$model_id ~ !.env$fit_succeeded
      ),
      prediction_written = dplyr::case_when(
        .default = prediction_written,
        model_id == .env$model_id & .env$fit_finished ~ FALSE
      ),
      last_prediction_date = dplyr::case_when(
        .default = as.character(last_prediction_date),
        model_id == .env$model_id & .env$fit_finished ~ NA_character_
      )
    )

  if (
    event == "fit_failed"
  ) {
    res_config <-
      compute_next_model_seed(
        data_config = res_config,
        model_ids = model_id,
        reason = "fit_failed"
      )
  }

  return(res_config)
}
