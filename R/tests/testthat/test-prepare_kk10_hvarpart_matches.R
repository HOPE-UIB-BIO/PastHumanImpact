testthat::test_that("KK10 matches preserve zero and land-cover fractions", {
  data_kk10 <-
    tibble::tibble(dataset_id = "a", age_bp = c(2000, 2500), value = c(0, 0.9))

  data_metadata <-
    tibble::tibble(dataset_id = "a", region = "Europe")

  res <-
    prepare_kk10_hvarpart_matches(data_kk10, data_metadata)

  testthat::expect_s3_class(res, "tbl_df")

  testthat::expect_named(
    res, c("dataset_id", "age_bp", "kk10_transformed", "region")
  )

  testthat::expect_identical(res[["kk10_transformed"]], c(0, 0.9))

  testthat::expect_identical(res[["region"]], rep("Europe", 2))

  testthat::expect_equal(
    nrow(prepare_kk10_hvarpart_matches(data_kk10[0, ], data_metadata)), 0L
  )
})

testthat::test_that("KK10 matches reject invalid inputs and missing regions", {
  data_kk10 <-
    tibble::tibble(dataset_id = "a", age_bp = 2000, value = 0.9)

  data_metadata <-
    tibble::tibble(dataset_id = "a", region = "Europe")

  for (invalid_value in c(-1, 1.1, NA_real_, Inf)) {
    testthat::expect_error(
      prepare_kk10_hvarpart_matches(
        dplyr::mutate(data_kk10, value = .env[["invalid_value"]]), data_metadata
      ),
      regexp = "finite fractions"
    )
  }

  testthat::expect_error(
    prepare_kk10_hvarpart_matches(
      dplyr::select(data_kk10, -dplyr::all_of("value")), data_metadata
    ),
    regexp = "unique keys"
  )

  testthat::expect_error(
    prepare_kk10_hvarpart_matches(
      dplyr::bind_rows(data_kk10, data_kk10), data_metadata
    ),
    regexp = "unique keys"
  )

  testthat::expect_error(
    prepare_kk10_hvarpart_matches(data_kk10, data_metadata[0, ]),
    regexp = "canonical region"
  )
})
