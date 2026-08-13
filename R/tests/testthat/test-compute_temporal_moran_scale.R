testthat::test_that(
  "compute_temporal_moran_scale() reports actual permutations",
  {
    ages <- seq(0, 3500, 500)
    permutations <- build_series_permutation_matrix(8L, 99L, 7L)
    result <-
      compute_temporal_moran_scale(
        values = seq_len(8),
        distance_matrix = compute_temporal_distance_matrix(ages),
        distance_years = 500,
        permutation_matrix = permutations
      )

    testthat::expect_equal(result$permutations, nrow(permutations))
    testthat::expect_true(is.finite(result$moran_i))
  }
)
