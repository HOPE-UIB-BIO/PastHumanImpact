testthat::test_that("save request appends one explicit authorization", {
  path_requests <-
    tempfile(fileext = ".csv")

  save_temporal_model_run_request(
    path = path_requests,
    model_id = "model_a",
    definition_hash = "hash_a",
    request_reason = "approved test",
    request_id = "request_a",
    requested_at = "2026-08-13"
  )

  result <-
    load_temporal_model_run_requests(path = path_requests)

  testthat::expect_identical(result[["request_id"]], "request_a")
  testthat::expect_true(result[["run_requested"]])
})

testthat::test_that("save request rejects duplicate request IDs", {
  path_requests <-
    tempfile(fileext = ".csv")

  save_temporal_model_run_request(
    path = path_requests,
    model_id = "model_a",
    definition_hash = "hash_a",
    request_reason = "approved test",
    request_id = "request_a"
  )

  testthat::expect_error(
    save_temporal_model_run_request(
      path = path_requests,
      model_id = "model_a",
      definition_hash = "hash_a",
      request_reason = "approved again",
      request_id = "request_a"
    ),
    regexp = "unique"
  )
})
