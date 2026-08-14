testthat::test_that(
  "compute_spatial_distance_matrix() returns great-circle distances in km",
  {
    data_input <-
      tibble::tibble(
        dataset_id = c("a", "b"),
        long = c(0, 1),
        lat = c(0, 0)
      )

    mat_result <-
      compute_spatial_distance_matrix(data_source = data_input)

    testthat::expect_true(is.matrix(mat_result))
    testthat::expect_equal(dim(mat_result), c(2L, 2L))
    testthat::expect_equal(unname(diag(mat_result)), c(0, 0))
    testthat::expect_equal(mat_result[1, 2], 111.32, tolerance = 0.05)
    testthat::expect_equal(rownames(mat_result), c("a", "b"))
  }
)

testthat::test_that(
  "compute_spatial_distance_matrix() validates required columns",
  {
    testthat::expect_error(
      compute_spatial_distance_matrix(
        data_source = tibble::tibble(dataset_id = 1, long = 0)
      ),
      "required contract"
    )
  }
)
