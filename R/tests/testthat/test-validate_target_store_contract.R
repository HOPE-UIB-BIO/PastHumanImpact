testthat::test_that("validate_target_store_contract() rejects missing store", {
  testthat::expect_error(
    validate_target_store_contract(
      store = tempfile(),
      target_names = "public_target",
      runner = "R/analyses/example/00_run.R"
    ),
    regexp = "Run"
  )
})

testthat::test_that("validate_target_store_contract() validates arguments", {
  testthat::expect_error(
    validate_target_store_contract(
      store = tempdir(),
      target_names = character(),
      runner = "runner.R"
    ),
    regexp = "invalid"
  )
})
