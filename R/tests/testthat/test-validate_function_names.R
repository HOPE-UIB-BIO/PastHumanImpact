testthat::test_that("function names satisfy the project contract", {
  validation <-
    validate_function_names()

  testthat::expect_true(
    all(dplyr::pull(validation, uses_approved_verb))
  )

  testthat::expect_true(
    all(dplyr::pull(validation, basename_matches))
  )
})

testthat::test_that("function-name validation rejects vague verbs", {
  function_dir <-
    file.path(tempdir(), "invalid-function-name")

  dir.create(
    function_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  writeLines(
    text = c(
      "get_value <- function(x) {",
      "  return(x)",
      "}"
    ),
    con = file.path(function_dir, "get_value.R")
  )

  testthat::expect_error(
    validate_function_names(function_dir = function_dir),
    "Function-name contract failed"
  )
})
