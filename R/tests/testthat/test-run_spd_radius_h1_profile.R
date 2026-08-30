testthat::test_that("run_spd_radius_h1_profile() rejects mixed radii", {
  predictors <-
    tibble::tibble(
      dataset_id = 1:2,
      radius_km = c(250L, 500L),
      spd_radius_specification = c("250_km", "500_km"),
      data_merge = list(tibble::tibble(), tibble::tibble())
    )

  testthat::expect_error(
    run_spd_radius_h1_profile(
      data_predictors_profile = predictors,
      data_properties_filtered = tibble::tibble(),
      data_meta = tibble::tibble(),
      response_vars = "response",
      predictor_vars = list(human = "spd", climate = "temp"),
      analysis_config = list(),
      data_profiles = tibble::tibble()
    ),
    regexp = "contract"
  )
})
