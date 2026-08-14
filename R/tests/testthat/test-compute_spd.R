testthat::test_that("compute_spd validates its data contract", {
  testthat::expect_error(
    compute_spd(
      data_source = tibble::tibble(),
      sel_dist = 100,
      sel_calcurve = tibble::tibble(),
      max_age = 1000,
      min_age = 0,
      sel_smooth_size = 100,
      min_n_dates = 50,
      normalise_to_one = FALSE
    ),
    "required contract"
  )
})
