testthat::test_that(
  "compute_temporal_distance_matrix() returns absolute age differences",
  {
    result <- compute_temporal_distance_matrix(c(0, 500, 1500))

    testthat::expect_equal(result[1, 3], 1500)
    testthat::expect_equal(result, t(result))
    testthat::expect_error(
      compute_temporal_distance_matrix(c(0, 0, 500)),
      "unique"
    )
  }
)
