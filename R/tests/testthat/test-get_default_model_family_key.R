testthat::test_that("get_default_model_family_key() maps model classes", {
  testthat::expect_identical(
    get_default_model_family_key("bi", "event_temporal"),
    "bernoulli_logit"
  )
  testthat::expect_identical(
    get_default_model_family_key("spd", "predictor_temporal"),
    "hurdle_gamma_log"
  )
  testthat::expect_identical(
    get_default_model_family_key("temp_annual", "predictor_temporal"),
    "gaussian_identity"
  )
  testthat::expect_identical(
    get_default_model_family_key("n0", "pap_temporal"),
    "student_identity"
  )
})

testthat::test_that("get_default_model_family_key() validates inputs", {
  testthat::expect_error(
    get_default_model_family_key(c("n0", "n1"), "pap_temporal"),
    regexp = "character scalars"
  )
})
