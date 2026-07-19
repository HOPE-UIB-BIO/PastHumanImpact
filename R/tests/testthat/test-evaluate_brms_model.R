testthat::test_that("evaluate_brms_model() flags missing model for rerun", {
  result <-
    evaluate_brms_model(NA_real_)

  testthat::expect_false(result[["last_run_rhat_test_pass"]])
  testthat::expect_false(result[["last_run_loo_test_pass"]])
  testthat::expect_true(all(is.na(result[["last_run_rhat_q90"]])))
  testthat::expect_true(all(is.na(result[["last_run_neff_ratio_min"]])))
  testthat::expect_true(all(is.na(result[["last_run_divergent_transitions"]])))
  testthat::expect_true(all(is.na(result[["last_run_max_treedepth_transitions"]])))
  testthat::expect_true(result[["need_to_run"]])
})

testthat::test_that("evaluate_brms_model() validates model input", {
  testthat::expect_error(
    evaluate_brms_model(list()),
    regexp = "brms model"
  )
})

testthat::test_that("LOO-only failure does not request a sampler rerun", {
  testthat::local_mocked_bindings(
    loo = function(...) {
      list(diagnostics = list(pareto_k = c(0.8, 0.9)))
    },
    rhat = function(...) c(1.00, 1.01),
    neff_ratio = function(...) c(0.5, 0.6),
    nuts_params = function(...) {
      tibble::tibble(
        Parameter = c("divergent__", "treedepth__"),
        Value = c(0, 5)
      )
    },
    .package = "brms"
  )

  mod <- structure(list(), class = "brmsfit")

  result <-
    evaluate_brms_model(
      mod = mod,
      pareto_k_threshold = 0.7,
      loo_threshold = 0.1,
      max_treedepth_threshold = 10L
    )

  testthat::expect_false(result[["last_run_loo_test_pass"]])
  testthat::expect_false(result[["need_to_run"]])
})
