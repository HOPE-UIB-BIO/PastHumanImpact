testthat::test_that("dataset SPD calculation validates its inputs", {
  testthat::expect_error(
    compute_dataset_spd(
      rc = tibble::tibble(),
      calcurve = tibble::tibble(),
      dataset_id = 1,
      dummy_age_table = tibble::tibble(),
      data_source_dist_vec = 100,
      min_n_dates = 50,
      age_to = 1000,
      age_from = 0,
      sel_smooth_size = 100,
      normalise_to_one = FALSE
    ),
    "required contract"
  )
})
