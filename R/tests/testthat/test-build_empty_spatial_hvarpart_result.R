testthat::test_that(
  "build_empty_spatial_hvarpart_result() returns the stable schema",
  {
    result <-
      build_empty_spatial_hvarpart_result(
        status = "missing_predictor_group",
        n_samples = 12L
      )

    testthat::expect_equal(result$status, "missing_predictor_group")
    testthat::expect_equal(result$n_samples, 12L)
    testthat::expect_null(result$human_climate_only_hvarpart)
    testthat::expect_equal(
      result$selection$status,
      "missing_predictor_group"
    )
  }
)
