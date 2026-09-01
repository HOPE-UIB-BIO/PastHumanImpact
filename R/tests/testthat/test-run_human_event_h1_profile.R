testthat::test_that(
  "run_human_event_h1_profile() rejects invalid scenario identifiers",
  {
    testthat::expect_error(
      run_human_event_h1_profile(
        data_predictors_cohort = data.frame(),
        cohort = "invalid",
        proxy_variant = "invalid",
        data_properties_filtered = data.frame(),
        data_meta = data.frame(),
        response_vars = character(),
        analysis_config = list(),
        data_profiles = data.frame()
      ),
      "identifiers are invalid"
    )
  }
)
