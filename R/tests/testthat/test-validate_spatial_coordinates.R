testthat::test_that(
  "validate_spatial_coordinates() identifies co-located records",
  {
    data_input <-
      tibble::tibble(
        dataset_id = 1:3,
        long = c(10, 10, 11),
        lat = c(50, 50, 51),
        region = c("A", "A", "A")
      )

    data_result <-
      validate_spatial_coordinates(
        data_source = data_input,
        group_col = "region"
      )

    testthat::expect_s3_class(data_result, "data.frame")
    testthat::expect_true(
      all(c("spatial_location_id", "n_colocated") %in% names(data_result))
    )
    testthat::expect_equal(
      dplyr::pull(data_result, "n_colocated"),
      c(2L, 2L, 1L)
    )
  }
)

testthat::test_that(
  "validate_spatial_coordinates() rejects invalid identifiers and coordinates",
  {
    data_duplicate <-
      tibble::tibble(
        dataset_id = c(1, 1),
        long = c(10, 11),
        lat = c(50, 51)
      )
    data_invalid <-
      tibble::tibble(
        dataset_id = 1:2,
        long = c(10, 181),
        lat = c(50, 51)
      )

    testthat::expect_error(
      validate_spatial_coordinates(data_source = data_duplicate),
      "Identifiers must be unique"
    )
    testthat::expect_error(
      validate_spatial_coordinates(data_source = data_invalid),
      "coordinates finite and valid"
    )
  }
)
