#' Prepare younger event-only time bins by chronology cohort
#'
#' @param data_timebins Canonical continent-by-age nested H1 input.
#' @param data_chronologies Dataset chronology audit with `have_events`.
#' @param cohort Internal cohort key.
#' @param upper_age_exclusive Upper age boundary for the younger extension.
#'
#' @return Nested continent-by-age input restricted to the requested cohort.
#'
#' @export
prepare_human_event_young_timebins <- function(
  data_timebins,
  data_chronologies,
  cohort,
  upper_age_exclusive = 2000
) {
  allowed_cohorts <- c("as_coded", "observed_events_only")
  assertthat::assert_that(
    is.data.frame(data_timebins),
    all(c("age", "region", "data_merge") %in% names(data_timebins)),
    is.list(data_timebins[["data_merge"]]),
    all(purrr::map_lgl(
      data_timebins[["data_merge"]],
      ~ is.data.frame(.x) && "dataset_id" %in% names(.x)
    )),
    is.data.frame(data_chronologies),
    all(c("dataset_id", "have_events") %in% names(data_chronologies)),
    is.logical(data_chronologies[["have_events"]]),
    !anyDuplicated(data_chronologies[["dataset_id"]]),
    cohort %in% allowed_cohorts,
    is.numeric(upper_age_exclusive),
    length(upper_age_exclusive) == 1L,
    is.finite(upper_age_exclusive),
    upper_age_exclusive > 0,
    msg = "Younger event-only time-bin inputs do not satisfy the contract."
  )

  retained_ids <-
    data_chronologies |>
    dplyr::filter(
      .data[["have_events"]] | .env[["cohort"]] == "as_coded"
    ) |>
    dplyr::pull(.data[["dataset_id"]])

  result <-
    data_timebins |>
    dplyr::filter(
      .data[["age"]] >= 0,
      .data[["age"]] < .env[["upper_age_exclusive"]]
    ) |>
    dplyr::mutate(
      data_merge = purrr::map(
        .data[["data_merge"]],
        ~ dplyr::filter(.x, .data[["dataset_id"]] %in% .env[["retained_ids"]])
      ),
      n_samples = purrr::map_int(.data[["data_merge"]], nrow)
    ) |>
    dplyr::filter(.data[["n_samples"]] > 0L)

  if (
    cohort == "observed_events_only" &&
      any(!purrr::map_lgl(
        result[["data_merge"]],
        ~ all(.x[["dataset_id"]] %in% retained_ids)
      ))
  ) {
    cli::cli_abort("The non-zero-event cohort retained an ineligible dataset.")
  }

  return(result)
}
