testthat::test_that("run-state transitions record and advance seeds", {
  data_config <-
    tibble::tibble(
      model_id = "model_a",
      seed_base = 1234L,
      seed_attempt = 1L,
      sampling_seed = 101L,
      seed_change_reason = "initial_model_seed",
      last_run_date = NA_character_,
      last_run_id = NA_character_,
      last_run_seed_attempt = NA_integer_,
      last_run_seed = NA_integer_,
      model_file_name = NA_character_,
      model_chain_seeds_json = NA_character_,
      model_seed_source = "not_fitted",
      model_provenance_status = "not_fitted",
      model_audit_reason = NA_character_,
      last_run_start_time = NA_character_,
      last_run_end_time = NA_character_,
      last_run_time = NA_character_,
      need_to_be_evaluated = FALSE,
      need_to_run = TRUE,
      prediction_written = TRUE,
      last_prediction_date = "2026-07-01"
    )

  run_start_time <-
    as.POSIXct("2026-07-15 10:00:00", tz = "UTC")

  result_started <-
    resolve_model_run_state(
      data_config = data_config,
      model_id = "model_a",
      run_id = "run_1",
      run_seed_attempt = 1L,
      run_seed = 101L,
      event = "fit_started",
      run_start_time = run_start_time,
      event_time = run_start_time
    )

  testthat::expect_identical(result_started[["last_run_id"]], "run_1")
  testthat::expect_true(is.na(result_started[["last_run_end_time"]]))
  testthat::expect_true(result_started[["need_to_run"]])

  result_failed <-
    resolve_model_run_state(
      data_config = result_started,
      model_id = "model_a",
      run_id = "run_1",
      run_seed_attempt = 1L,
      run_seed = 101L,
      event = "fit_failed",
      run_start_time = run_start_time,
      event_time = run_start_time + 60
    )

  testthat::expect_true(result_failed[["need_to_run"]])
  testthat::expect_false(result_failed[["need_to_be_evaluated"]])
  testthat::expect_identical(result_failed[["last_run_seed"]], 101L)
  testthat::expect_identical(result_failed[["seed_attempt"]], 2L)
  testthat::expect_false(result_failed[["sampling_seed"]] == 101L)
})

testthat::test_that("successful run-state transitions queue evaluation", {
  data_config <-
    tibble::tibble(
      model_id = "model_a",
      last_run_date = NA_character_,
      last_run_id = NA_character_,
      last_run_seed_attempt = NA_integer_,
      last_run_seed = NA_integer_,
      model_file_name = NA_character_,
      model_chain_seeds_json = NA_character_,
      model_seed_source = "not_fitted",
      model_provenance_status = "not_fitted",
      model_audit_reason = NA_character_,
      last_run_start_time = NA_character_,
      last_run_end_time = NA_character_,
      last_run_time = NA_character_,
      need_to_be_evaluated = FALSE,
      need_to_run = TRUE,
      prediction_written = TRUE,
      last_prediction_date = "2026-07-01"
    )

  run_start_time <-
    as.POSIXct("2026-07-15 10:00:00", tz = "UTC")

  result <-
    resolve_model_run_state(
      data_config = data_config,
      model_id = "model_a",
      run_id = "run_1",
      run_seed_attempt = 1L,
      run_seed = 101L,
      model_file_name = "model_a__attempt__1.qs",
      model_chain_seeds_json = '{"chain_1":101}',
      event = "fit_succeeded",
      run_start_time = run_start_time,
      event_time = run_start_time + 60
    )

  testthat::expect_false(result[["need_to_run"]])
  testthat::expect_true(result[["need_to_be_evaluated"]])
  testthat::expect_false(result[["prediction_written"]])
  testthat::expect_true(is.na(result[["last_prediction_date"]]))
  testthat::expect_identical(
    result[["model_file_name"]],
    "model_a__attempt__1.qs"
  )
  testthat::expect_identical(
    result[["model_chain_seeds_json"]],
    '{"chain_1":101}'
  )
  testthat::expect_identical(
    result[["model_provenance_status"]],
    "configured_run_recorded"
  )
})
