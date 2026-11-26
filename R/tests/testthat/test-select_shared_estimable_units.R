testthat::test_that("select_shared_estimable_units() intersects runs", {
  data_units <-
    tibble::tibble(
      profile_id = c("spd", "spd", "spd", "kk10", "kk10", "hyde", "hyde"),
      dataset_id = c("1", "2", "3", "2", "3", "3", "2")
    )

  res <-
    select_shared_estimable_units(
      data_units = data_units,
      unit_columns = "dataset_id",
      required_groups = c("spd", "kk10", "hyde")
    )

  testthat::expect_s3_class(res, "tbl_df")
  testthat::expect_named(res, "dataset_id")
  testthat::expect_equal(dplyr::pull(res, "dataset_id"), c("2", "3"))
})

testthat::test_that("select_shared_estimable_units() ignores other runs", {
  data_units <-
    tibble::tibble(
      profile_id = c("spd", "kk10", "other"),
      region = "A",
      age = c(2000, 2000, 2500)
    )

  res <-
    select_shared_estimable_units(
      data_units = data_units,
      unit_columns = c("region", "age"),
      required_groups = c("spd", "kk10")
    )

  testthat::expect_equal(base::nrow(res), 1L)
  testthat::expect_equal(dplyr::pull(res, "age"), 2000)
})

testthat::test_that("select_shared_estimable_units() needs every run", {
  data_units <-
    tibble::tibble(profile_id = "spd", dataset_id = "1")

  testthat::expect_error(
    select_shared_estimable_units(
      data_units = data_units,
      unit_columns = "dataset_id",
      required_groups = c("spd", "kk10")
    ),
    regexp = "Shared-unit inputs"
  )
})
