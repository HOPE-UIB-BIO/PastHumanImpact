testthat::test_that("missing request ledgers load as empty", {
  result <-
    load_temporal_model_run_requests(path = tempfile())

  testthat::expect_s3_class(result, "data.frame")
  testthat::expect_equal(nrow(result), 0L)
})

testthat::test_that("request ledgers retain logical authorization", {
  path_requests <-
    tempfile(fileext = ".csv")

  readr::write_csv(
    tibble::tibble(
      request_id = "request_a",
      model_id = "model_a",
      definition_hash = "hash_a",
      run_requested = TRUE,
      request_reason = "approved",
      requested_at = "2026-08-13"
    ),
    path_requests
  )

  result <-
    load_temporal_model_run_requests(path = path_requests)

  testthat::expect_true(result[["run_requested"]])
})
