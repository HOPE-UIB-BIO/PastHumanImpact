testthat::test_that(
  "compute_moran_i() returns a finite statistic",
  {
    mat_weights <-
      matrix(
        c(0, 1, 0, 1, 0, 1, 0, 1, 0),
        nrow = 3,
        byrow = TRUE
      )

    result_value <-
      compute_moran_i(
        values = c(1, 2, 3),
        weights = mat_weights
      )

    testthat::expect_type(result_value, "double")
    testthat::expect_true(is.finite(result_value))
  }
)

testthat::test_that(
  "compute_moran_i() returns NA without information",
  {
    testthat::expect_true(
      is.na(
        compute_moran_i(
          values = c(1, 1),
          weights = matrix(c(0, 1, 1, 0), nrow = 2)
        )
      )
    )
  }
)

testthat::test_that(
  "compute_moran_i() validates dimensions",
  {
    testthat::expect_error(
      compute_moran_i(
        values = 1:3,
        weights = matrix(1, nrow = 2, ncol = 2)
      ),
      "contract"
    )
  }
)
