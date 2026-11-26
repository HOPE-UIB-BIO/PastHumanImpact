testthat::test_that("validate_human_proxy_common_units() accepts equal sets", {
  data_run_units <-
    tidyr::expand_grid(
      profile_id = c("spd_run", "kk10_run"),
      region = "A",
      age = c(2000, 2500)
    )

  res <-
    validate_human_proxy_common_units(
      data_run_units = data_run_units,
      unit_columns = c("region", "age")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, c("profile_id", "n_units", "identical_units"))
  testthat::expect_equal(dplyr::pull(res, "n_units"), c(2L, 2L))
  testthat::expect_true(base::all(dplyr::pull(res, "identical_units")))
})

testthat::test_that("validate_human_proxy_common_units() stops on change", {
  data_run_units <-
    tibble::tibble(
      profile_id = c("spd_run", "spd_run", "kk10_run"),
      dataset_id = c("1", "2", "1")
    )

  testthat::expect_error(
    validate_human_proxy_common_units(
      data_run_units = data_run_units,
      unit_columns = "dataset_id"
    ),
    regexp = "identical units"
  )
})

testthat::test_that("validate_human_proxy_common_units() validates inputs", {
  testthat::expect_error(
    validate_human_proxy_common_units(
      data_run_units = tibble::tibble(dataset_id = "1"),
      unit_columns = "dataset_id"
    ),
    regexp = "Common-unit check inputs"
  )
})
