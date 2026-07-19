testthat::test_that("get_temporal_model_analysis_names() maps model groups", {
  testthat::expect_identical(
    get_temporal_model_analysis_names("predictors"),
    "predictor_temporal"
  )
  testthat::expect_identical(
    get_temporal_model_analysis_names("events"),
    "event_temporal"
  )
  testthat::expect_identical(
    get_temporal_model_analysis_names("paps"),
    "pap_temporal"
  )
  testthat::expect_identical(
    get_temporal_model_analysis_names("all"),
    c("predictor_temporal", "event_temporal", "pap_temporal")
  )
})

testthat::test_that("get_temporal_model_analysis_names() validates type", {
  testthat::expect_error(
    get_temporal_model_analysis_names("bad"),
    regexp = "should be one of"
  )
})
