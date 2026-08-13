testthat::test_that("build_model_run_event() records reproducibility fields", {
  model_config_row <-
    tibble::tibble(
      model_id = "model_a",
      analysis = "pap_temporal",
      variable = "n0",
      region = "Europe",
      climatezone = "Temperate",
      family_key = "gamma_log",
      model_profile = "within_stratum_dataset_fs",
      formula_text = "value ~ age_ka_scaled",
      total_iterations = 6400L,
      min_iterations_per_chain = 100L,
      max_chains = 4L,
      adapt_delta = 0.99,
      max_treedepth = 12L,
      seed_attempt = 2L,
      sampling_seed = 456L
    )

  result <-
    build_model_run_event(
      model_config_row = model_config_row,
      run_id = "model_a__attempt_2",
      event = "fit_started",
      event_time = as.POSIXct("2026-07-15 12:00:00", tz = "UTC"),
      git_commit = "abc123",
      git_is_dirty = FALSE
    )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_identical(result[["run_seed"]], 456L)
  testthat::expect_identical(result[["run_seed_attempt"]], 2L)
  testthat::expect_identical(result[["event"]], "fit_started")
  testthat::expect_match(result[["config_snapshot_json"]], "gamma_log")
})

testthat::test_that("build_model_run_event() validates one config row", {
  testthat::expect_error(
    build_model_run_event(
      model_config_row = tibble::tibble(model_id = c("a", "b")),
      run_id = "run",
      event = "fit_started"
    ),
    regexp = "one-row"
  )
})
