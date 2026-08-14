testthat::test_that("resolve_default_model_family_key() maps model classes", {
  testthat::expect_identical(
    resolve_default_model_family_key("bi", "event_temporal"),
    "bernoulli_logit"
  )
  testthat::expect_identical(
    resolve_default_model_family_key("spd", "predictor_temporal"),
    "hurdle_gamma_log"
  )
  testthat::expect_identical(
    resolve_default_model_family_key("temp_annual", "predictor_temporal"),
    "gaussian_identity"
  )
  testthat::expect_identical(
    resolve_default_model_family_key("n0", "pap_temporal"),
    "gamma_log"
  )
  testthat::expect_identical(
    resolve_default_model_family_key("n1_minus_n2", "pap_temporal"),
    "hurdle_gamma_log"
  )
  testthat::expect_identical(
    resolve_default_model_family_key("dcca_axis_1", "pap_temporal"),
    "hurdle_gamma_log"
  )
  testthat::expect_identical(
    resolve_default_model_family_key("density_diversity", "pap_temporal"),
    "zero_one_inflated_beta_logit"
  )
})

testthat::test_that("resolve_default_model_family_key() validates inputs", {
  testthat::expect_error(
    resolve_default_model_family_key(c("n0", "n1"), "pap_temporal"),
    regexp = "character scalars"
  )
})
