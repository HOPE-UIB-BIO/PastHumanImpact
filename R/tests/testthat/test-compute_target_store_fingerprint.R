testthat::test_that(
  "compute_target_store_fingerprint() rejects a missing store",
  {
  testthat::expect_error(
    compute_target_store_fingerprint(
      store = tempfile(),
      target_names = "public_target",
      runner = "runner.R"
    ),
    regexp = "does not exist"
  )
  }
)
