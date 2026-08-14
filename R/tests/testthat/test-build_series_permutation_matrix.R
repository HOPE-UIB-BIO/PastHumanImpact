testthat::test_that(
  "build_series_permutation_matrix() is deterministic and ordered",
  {
    first <- build_series_permutation_matrix(8L, 99L, seed = 42L)
    second <- build_series_permutation_matrix(8L, 99L, seed = 42L)

    testthat::expect_identical(first, second)
    testthat::expect_equal(ncol(first), 8L)
    testthat::expect_true(
      all(
        purrr::map_lgl(
          seq_len(nrow(first)),
          ~ identical(sort(first[.x, ]), seq_len(8L))
        )
      )
    )
  }
)
