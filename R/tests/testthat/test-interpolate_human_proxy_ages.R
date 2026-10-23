testthat::test_that(
  "interpolate_human_proxy_ages() interpolates without extrapolation",
  {
  source <-
    tibble::tibble(
      dataset_id = "a",
      proxy = "hyde",
      age_bp = c(1000, 2000),
      value = c(2, 6)
    )

  result <-
    interpolate_human_proxy_ages(source, c(500, 1000, 1500, 2000, 2500))

  testthat::expect_equal(result[["value"]], c(NA, 2, 4, 6, NA))
  testthat::expect_equal(result[["interpolation_weight"]][3], 0.5)
})
