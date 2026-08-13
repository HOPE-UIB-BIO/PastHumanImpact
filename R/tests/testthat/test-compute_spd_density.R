testthat::test_that("SPD density returns zero when dates are insufficient", {
  data_source <-
    tibble::tibble(
      dist = 10,
      Age = 500,
      Error = 20,
      LabID = "one"
    )

  calibration_curve <-
    tibble::tibble(C14BP = 0)

  result <-
    compute_spd_density(
      data_source = data_source,
      sel_dist = 100,
      sel_calcurve = calibration_curve,
      min_n_dates = 1,
      max_age = 1000,
      min_age = 0,
      sel_smooth_size = 100,
      normalise_to_one = FALSE
    )

  testthat::expect_identical(result, 0)
})
