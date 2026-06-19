testthat::test_that("get_model_family() returns known families", {
  testthat::skip_if_not_installed("brms")

  result <- get_model_family("student_identity")

  testthat::expect_true(inherits(result, "family"))
  testthat::expect_identical(result[["family"]], "student")
  testthat::expect_identical(result[["link"]], "identity")
})

testthat::test_that("get_model_family() rejects unknown keys", {
  testthat::skip_if_not_installed("brms")

  testthat::expect_error(
    get_model_family("not_a_family"),
    regexp = "Unknown"
  )
})
