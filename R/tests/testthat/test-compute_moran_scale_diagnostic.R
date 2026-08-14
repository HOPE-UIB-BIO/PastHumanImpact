testthat::test_that(
  "compute_moran_scale_diagnostic() returns one diagnostic row",
  {
    data_input <-
      tibble::tibble(response = c(1, 2, 3, 4))
    mat_distance <-
      matrix(
        c(
          0, 100, 300, 400,
          100, 0, 100, 300,
          300, 100, 0, 100,
          400, 300, 100, 0
        ),
        nrow = 4,
        byrow = TRUE
      )

    data_result <-
      compute_moran_scale_diagnostic(
        data_source = data_input,
        value_col = "response",
        distance_matrix = mat_distance,
        distance_km = 150,
        blocks = rep("all", 4),
        permutations = 19L
      )

    testthat::expect_s3_class(data_result, "tbl_df")
    testthat::expect_equal(nrow(data_result), 1L)
    testthat::expect_identical(dplyr::pull(data_result, "value"), "response")
  }
)

testthat::test_that(
  "compute_moran_scale_diagnostic() validates the value column",
  {
    testthat::expect_error(
      compute_moran_scale_diagnostic(
        data_source = tibble::tibble(response = 1:3),
        value_col = "missing",
        distance_matrix = matrix(0, nrow = 3, ncol = 3),
        distance_km = 250,
        blocks = rep("all", 3),
        permutations = 9L
      ),
      "contract"
    )
  }
)
