testthat::test_that("initialization creates an empty request ledger", {
  path_requests <-
    file.path(tempfile(), "general_model_run_requests.csv")

  initialize_temporal_model_run_requests(path = path_requests)

  result <-
    load_temporal_model_run_requests(path = path_requests)

  testthat::expect_true(file.exists(path_requests))
  testthat::expect_equal(nrow(result), 0L)
})
