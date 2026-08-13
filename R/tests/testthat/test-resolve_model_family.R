testthat::test_that("resolve_model_family() returns known families", {
  testthat::skip_if_not_installed("brms")

  result <- resolve_model_family("student_identity")

  testthat::expect_true(inherits(result, "family"))
  testthat::expect_identical(result[["family"]], "student")
  testthat::expect_identical(result[["link"]], "identity")

  result_gamma <- resolve_model_family("gamma_log")
  result_beta <- resolve_model_family("zero_one_inflated_beta_logit")

  testthat::expect_identical(result_gamma[["family"]], "gamma")
  testthat::expect_identical(result_gamma[["link"]], "log")
  testthat::expect_identical(
    result_beta[["family"]],
    "zero_one_inflated_beta"
  )
  testthat::expect_identical(result_beta[["link"]], "logit")
})

testthat::test_that("resolve_model_family() rejects unknown keys", {
  testthat::skip_if_not_installed("brms")

  testthat::expect_error(
    resolve_model_family("not_a_family"),
    regexp = "Unknown"
  )
})
