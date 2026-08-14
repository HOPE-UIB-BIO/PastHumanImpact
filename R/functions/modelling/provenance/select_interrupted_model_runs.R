#' @title Identify interrupted temporal model runs
#' @description
#' Return fit-start events that have no terminal fit or evaluation event with
#' the same run ID.
#' @param data_history Model run-history data frame.
#' @return Tibble containing unmatched fit-start events.
#' @examples
#' dontrun{
#' interrupted <- select_interrupted_model_runs(data_history = run_history)
#' }
select_interrupted_model_runs <- function(
  data_history
) {
  assertthat::assert_that(
    is.data.frame(data_history),
    msg = "`data_history` must be a data frame."
  )

  required_columns <-
    c(
      "run_id",
      "event",
      "event_time",
      "model_id",
      "run_seed_attempt",
      "run_seed"
    )

  assertthat::assert_that(
    all(required_columns %in% names(data_history)),
    msg = "`data_history` is missing required run event columns."
  )

  terminal_events <-
    c(
      "fit_succeeded",
      "fit_failed",
      "fit_interrupted",
      "evaluation_passed",
      "evaluation_failed"
    )

  terminal_run_ids <-
    data_history |>
    dplyr::filter(event %in% terminal_events) |>
    dplyr::pull(run_id) |>
    unique()

  res_interrupted <-
    data_history |>
    dplyr::filter(
      event == "fit_started",
      !run_id %in% terminal_run_ids
    ) |>
    dplyr::arrange(event_time) |>
    dplyr::distinct(run_id, .keep_all = TRUE)

  return(res_interrupted)
}
