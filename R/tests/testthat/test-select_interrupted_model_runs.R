testthat::test_that("select_interrupted_model_runs() finds unmatched starts", {
  data_history <-
    tibble::tibble(
      run_id = c("run_1", "run_1", "run_2"),
      event = c("fit_started", "fit_succeeded", "fit_started"),
      event_time = c("2026-07-15T10:00:00Z", "2026-07-15T10:01:00Z",
        "2026-07-15T11:00:00Z"),
      model_id = c("model_a", "model_a", "model_b"),
      run_seed_attempt = c(1L, 1L, 2L),
      run_seed = c(101L, 101L, 202L)
    )

  result <-
    select_interrupted_model_runs(data_history = data_history)

  testthat::expect_identical(result[["run_id"]], "run_2")
  testthat::expect_identical(result[["model_id"]], "model_b")
})

testthat::test_that("evaluation events terminate interrupted runs", {
  data_history <-
    tibble::tibble(
      run_id = c("run_1", "run_1"),
      event = c("fit_started", "evaluation_passed"),
      event_time = c("2026-07-15T10:00:00Z", "2026-07-15T10:02:00Z"),
      model_id = "model_a",
      run_seed_attempt = 1L,
      run_seed = 101L
    )

  result <-
    select_interrupted_model_runs(data_history = data_history)

  testthat::expect_identical(nrow(result), 0L)
})
