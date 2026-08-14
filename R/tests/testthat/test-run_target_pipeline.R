testthat::test_that("run_target_pipeline() validates its script", {
  testthat::expect_error(
    run_target_pipeline(
      script = tempfile(),
      store = tempfile()
    ),
    regexp = "invalid"
  )
})
