testthat::test_that(
  "compute_temporal_moran_diagnostics() includes connectivity",
  {
    data_input <-
      tibble::tibble(
        age = c(0, 500, 1000, 2000, 2500, 3000),
        residual_axis_1 = c(1, 2, 3, 3, 2, 1)
      )
    result <-
      compute_temporal_moran_diagnostics(
        data_source = data_input,
        value_cols = "residual_axis_1",
        distance_years = 500,
        permutations = 19L
      )

    testthat::expect_setequal(
      result$temporal_scope,
      c("fixed_500y", "connectivity")
    )
    testthat::expect_true(all(result$permutations <= 19L))
  }
)
