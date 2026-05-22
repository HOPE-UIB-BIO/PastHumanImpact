testthat::test_that("check_function_contract_coverage() reports coverage table", {
  root_dir <-
    file.path(tempdir(), "coverage-gate-ok")

  function_dir <-
    file.path(root_dir, "R", "functions", "events")
  test_dir <-
    file.path(root_dir, "R", "tests", "testthat")

  dir.create(function_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    con = file.path(function_dir, "foo_function.R"),
    text = c(
      "#' @title Foo",
      "foo_function <- function(x) {",
      "  assertthat::assert_that(is.numeric(x))",
      "  return(x)",
      "}"
    )
  )

  writeLines(
    con = file.path(test_dir, "test-foo_function.R"),
    text = c(
      "testthat::test_that('foo', {",
      "  testthat::expect_true(TRUE)",
      "})"
    )
  )

  coverage <-
    check_function_contract_coverage(
      function_dir = file.path(root_dir, "R", "functions"),
      test_dir = test_dir,
      fail_on_missing = FALSE
    )

  testthat::expect_equal(nrow(coverage), 1L)
  testthat::expect_true(all(c("has_roxygen", "has_validation", "has_test_file") %in% names(coverage)))
  testthat::expect_true(coverage$has_roxygen[[1]])
  testthat::expect_true(coverage$has_validation[[1]])
  testthat::expect_true(coverage$has_test_file[[1]])
})

testthat::test_that("check_function_contract_coverage() can fail on missing contracts", {
  root_dir <-
    file.path(tempdir(), "coverage-gate-missing")

  function_dir <-
    file.path(root_dir, "R", "functions", "events")
  test_dir <-
    file.path(root_dir, "R", "tests", "testthat")

  dir.create(function_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    con = file.path(function_dir, "bar_function.R"),
    text = c(
      "bar_function <- function(x) {",
      "  return(x)",
      "}"
    )
  )

  testthat::expect_error(
    check_function_contract_coverage(
      function_dir = file.path(root_dir, "R", "functions"),
      test_dir = test_dir,
      fail_on_missing = TRUE
    ),
    regexp = "Contract coverage gate failed"
  )
})

testthat::test_that("check_function_contract_coverage() excludes testing helpers only", {
  root_dir <-
    file.path(tempdir(), "coverage-gate-testing-exclusion")

  function_dir_events <-
    file.path(root_dir, "R", "functions", "events")
  function_dir_testing <-
    file.path(root_dir, "R", "functions", "testing")
  test_dir <-
    file.path(root_dir, "R", "tests", "testthat")

  dir.create(function_dir_events, recursive = TRUE, showWarnings = FALSE)
  dir.create(function_dir_testing, recursive = TRUE, showWarnings = FALSE)
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  writeLines(
    con = file.path(function_dir_events, "kept_function.R"),
    text = c(
      "#' @title Kept",
      "kept_function <- function(x) {",
      "  assertthat::assert_that(is.numeric(x))",
      "  return(x)",
      "}"
    )
  )

  writeLines(
    con = file.path(function_dir_testing, "helper_function.R"),
    text = c(
      "#' @title Helper",
      "helper_function <- function(x) {",
      "  assertthat::assert_that(is.numeric(x))",
      "  return(x)",
      "}"
    )
  )

  writeLines(
    con = file.path(test_dir, "test-kept_function.R"),
    text = c(
      "testthat::test_that('kept', {",
      "  testthat::expect_true(TRUE)",
      "})"
    )
  )

  coverage_table <-
    check_function_contract_coverage(
      function_dir = file.path(root_dir, "R", "functions"),
      test_dir = test_dir,
      fail_on_missing = FALSE
    )

  testthat::expect_equal(nrow(coverage_table), 1L)
  testthat::expect_identical(
    dplyr::pull(coverage_table, function_name),
    "kept_function"
  )
})
