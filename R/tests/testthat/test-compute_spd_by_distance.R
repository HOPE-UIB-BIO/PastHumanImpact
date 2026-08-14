testthat::test_that("SPD calculation rejects incomplete input data", {
  testthat::expect_error(
    compute_spd_by_distance(
      data_source_c14 = tibble::tibble(),
      data_source_dist_vec = 1000
    )
  )
})
