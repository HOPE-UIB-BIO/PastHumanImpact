testthat::test_that("evaluate_brms_model() flags missing model for rerun", {
  result <-
    evaluate_brms_model(NA_real_)

  testthat::expect_false(result[["last_run_rhat_test_pass"]])
  testthat::expect_false(result[["last_run_loo_test_pass"]])
  testthat::expect_true(result[["need_to_run"]])
})

testthat::test_that("evaluate_brms_model() validates model input", {
  testthat::expect_error(
    evaluate_brms_model(list()),
    regexp = "brms model"
  )
})
