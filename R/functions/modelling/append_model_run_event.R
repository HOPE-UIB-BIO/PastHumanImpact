#' @title Append an event to temporal model run history
#' @description
#' Append one or more model run events to a stable CSV without replacing
#' previous attempts.
#' @param data_event Data frame containing model run-history events.
#' @param path_history Character scalar output CSV path.
#' @return Invisibly returns the normalised history path.
#' @examples
#' dontrun{
#' append_model_run_event(
#'   data_event = event,
#'   path_history = "Data/Temporal_models/model_run_history.csv"
#' )
#' }
append_model_run_event <- function(
  data_event,
  path_history
) {
  assertthat::assert_that(
    is.data.frame(data_event),
    nrow(data_event) > 0L,
    msg = "`data_event` must be a non-empty data frame."
  )
  assertthat::assert_that(
    is.character(path_history),
    length(path_history) == 1L,
    !is.na(path_history),
    nzchar(path_history),
    msg = "`path_history` must be a non-empty character scalar."
  )

  history_exists <-
    file.exists(path_history)

  if (
    isTRUE(history_exists)
  ) {
    history_columns <-
      readr::read_csv(
        path_history,
        n_max = 0L,
        show_col_types = FALSE
      ) |>
      names()

    assertthat::assert_that(
      identical(history_columns, names(data_event)),
      msg = "Run-history event columns do not match the existing CSV columns."
    )
  } else {
    dir.create(
      dirname(path_history),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  readr::write_csv(
    data_event,
    path_history,
    append = history_exists,
    col_names = !history_exists,
    na = "NA"
  )

  res_path <-
    normalizePath(
      path_history,
      winslash = "/",
      mustWork = TRUE
    )

  return(invisible(res_path))
}
