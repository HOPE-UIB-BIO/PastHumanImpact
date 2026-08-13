testthat::test_that(
  "select_spatial_thinning_stratum() respects the minimum distance",
  {
    mat_distance <-
      matrix(
        c(0, 100, 500, 100, 0, 400, 500, 400, 0),
        nrow = 3,
        byrow = TRUE
      )
    set.seed(900723)

    vec_result <-
      select_spatial_thinning_stratum(
        row_indices = 1:3,
        distance_matrix = mat_distance,
        distance_km = 250
      )

    testthat::expect_true(length(vec_result) >= 2L)
    testthat::expect_true(
      all(mat_distance[vec_result, vec_result][upper.tri(
        mat_distance[vec_result, vec_result]
      )] >= 250)
    )
  }
)

testthat::test_that(
  "select_spatial_thinning_stratum() validates row indices",
  {
    testthat::expect_error(
      select_spatial_thinning_stratum(
        row_indices = 4L,
        distance_matrix = matrix(0, nrow = 3, ncol = 3),
        distance_km = 250
      ),
      "contract"
    )
  }
)
