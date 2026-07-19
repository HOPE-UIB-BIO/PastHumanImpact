testthat::test_that("append_model_run_event() appends without replacing", {
  path_history <-
    tempfile(fileext = ".csv")

  data_event <-
    tibble::tibble(
      run_id = "run_1",
      event = "fit_started"
    )

  append_model_run_event(
    data_event = data_event,
    path_history = path_history
  )
  append_model_run_event(
    data_event = dplyr::mutate(data_event, event = "fit_succeeded"),
    path_history = path_history
  )

  result <-
    readr::read_csv(
      path_history,
      show_col_types = FALSE
    )

  testthat::expect_identical(nrow(result), 2L)
  testthat::expect_identical(
    result[["event"]],
    c("fit_started", "fit_succeeded")
  )
})

testthat::test_that("append_model_run_event() validates columns", {
  path_history <-
    tempfile(fileext = ".csv")

  append_model_run_event(
    data_event = tibble::tibble(run_id = "run_1", event = "fit_started"),
    path_history = path_history
  )

  testthat::expect_error(
    append_model_run_event(
      data_event = tibble::tibble(run_id = "run_2"),
      path_history = path_history
    ),
    regexp = "columns"
  )
})
