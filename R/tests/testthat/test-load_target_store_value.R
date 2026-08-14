testthat::test_that("load_target_store_value() rejects missing store", {
  testthat::expect_error(
    load_target_store_value(
      store = tempfile(),
      target_name = "public_target",
      runner = "runner.R"
    ),
    regexp = "does not exist"
  )
})
