testthat::test_that("HYDE matches preserve zero and square-root population", {
  data_hyde <-
    tibble::tibble(dataset_id = "a", age_bp = c(2000, 2500), value = c(0, 9))

  data_metadata <-
    tibble::tibble(dataset_id = "a", region = "Europe")

  res <-
    prepare_hyde_hvarpart_matches(data_hyde, data_metadata)

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res, c("dataset_id", "age_bp", "hyde_transformed", "region")
  )

  testthat::expect_identical(res[["hyde_transformed"]], c(0, 3))

  testthat::expect_identical(res[["region"]], rep("Europe", 2))

  testthat::expect_equal(
    nrow(prepare_hyde_hvarpart_matches(data_hyde[0, ], data_metadata)), 0L
  )
})

testthat::test_that("HYDE matches reject invalid inputs and missing regions", {
  data_hyde <-
    tibble::tibble(dataset_id = "a", age_bp = 2000, value = 9)

  data_metadata <-
    tibble::tibble(dataset_id = "a", region = "Europe")

  for (invalid_value in c(-1, NA_real_, Inf)) {
    testthat::expect_error(
      prepare_hyde_hvarpart_matches(
        dplyr::mutate(data_hyde, value = .env[["invalid_value"]]), data_metadata
      ),
      regexp = "finite non-negative"
    )
  }

  testthat::expect_error(
    prepare_hyde_hvarpart_matches(
      dplyr::select(data_hyde, -dplyr::all_of("value")), data_metadata
    ),
    regexp = "unique keys"
  )

  testthat::expect_error(
    prepare_hyde_hvarpart_matches(
      dplyr::bind_rows(data_hyde, data_hyde), data_metadata
    ),
    regexp = "unique keys"
  )

  testthat::expect_error(
    prepare_hyde_hvarpart_matches(data_hyde, data_metadata[0, ]),
    regexp = "canonical region"
  )
})
