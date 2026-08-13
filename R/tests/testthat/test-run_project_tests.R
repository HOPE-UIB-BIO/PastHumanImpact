testthat::test_that("project test runner validates its directory", {
  testthat::expect_error(
    run_project_tests(test_dir = character()),
    "single directory path"
  )

  testthat::expect_error(
    run_project_tests(test_dir = tempfile("missing-test-directory-")),
    "existing directory"
  )
})
