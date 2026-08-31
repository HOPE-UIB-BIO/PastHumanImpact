testthat::test_that(
  "run_h1_control_profile() validates reusable-runner inputs",
  {
    testthat::expect_error(
      run_h1_control_profile(
        data_predictors_profile = data.frame(),
        data_properties_filtered = data.frame(),
        data_meta = data.frame(),
        response_vars = character(),
        predictor_vars = list(),
        analysis_config = list(),
        data_profiles = data.frame()
      ),
      "contract"
    )
  }
)
