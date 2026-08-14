testthat::test_that("run-request validation accepts an empty ledger", {
  data_requests <-
    tibble::tibble(
      request_id = character(),
      model_id = character(),
      definition_hash = character(),
      run_requested = logical(),
      request_reason = character(),
      requested_at = character()
    )

  testthat::expect_silent(
    validate_temporal_model_run_requests(data_requests = data_requests)
  )
})

testthat::test_that("run-request validation rejects duplicate IDs", {
  data_requests <-
    tibble::tibble(
      request_id = c("request_a", "request_a"),
      model_id = c("model_a", "model_b"),
      definition_hash = c("hash_a", "hash_b"),
      run_requested = TRUE,
      request_reason = "test",
      requested_at = "2026-08-13"
    )

  testthat::expect_error(
    validate_temporal_model_run_requests(data_requests = data_requests),
    regexp = "unique"
  )
})
