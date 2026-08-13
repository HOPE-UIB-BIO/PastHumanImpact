testthat::test_that("test-file runner validates its path", {
  testthat::expect_error(
    run_test_file(test_file = character()),
    "single file path"
  )

  testthat::expect_error(
    run_test_file(test_file = tempfile("missing-test-file-")),
    "does not exist"
  )
})
