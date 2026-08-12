testthat::test_that(
  "build_dbmem_group_basis() expands co-located records",
  {
    data_input <-
      tibble::tibble(
        dataset_id = stringr::str_c("site_", 1:7),
        long = c(0, 0, 1, 2, 3, 4, 5),
        lat = c(0, 0, 1, 0, 1, 0, 1)
      ) |>
      validate_spatial_coordinates()

    result_group <-
      build_dbmem_group_basis(
        data_group = data_input,
        group_value = "all",
        long_col = "long",
        lat_col = "lat",
        min_unique_locations = 3L
      )

    testthat::expect_true(is.matrix(result_group[["basis"]]))
    testthat::expect_equal(nrow(result_group[["basis"]]), 7L)
    testthat::expect_equal(
      result_group[["basis"]][1, , drop = FALSE],
      result_group[["basis"]][2, , drop = FALSE]
    )
    testthat::expect_equal(nrow(result_group[["diagnostic"]]), 1L)
  }
)

testthat::test_that(
  "build_dbmem_group_basis() reports insufficient locations",
  {
    data_input <-
      tibble::tibble(
        dataset_id = c("a", "b"),
        long = c(0, 1),
        lat = c(0, 1)
      ) |>
      validate_spatial_coordinates()

    result_group <-
      build_dbmem_group_basis(
        data_group = data_input,
        group_value = "all",
        long_col = "long",
        lat_col = "lat",
        min_unique_locations = 3L
      )

    testthat::expect_equal(ncol(result_group[["basis"]]), 0L)
    testthat::expect_identical(
      dplyr::pull(result_group[["diagnostic"]], "status"),
      "insufficient_locations"
    )
  }
)
