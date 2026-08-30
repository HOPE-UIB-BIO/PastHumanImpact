testthat::test_that("summarise_spd_radius_coverage() reports geography", {
  data_spd <-
    tidyr::expand_grid(
      dataset_id = 1:2,
      radius_km = c(250L, 500L)
    ) |>
    dplyr::mutate(
      n_time_points = 2L,
      n_finite_values = 2L,
      available = c(TRUE, TRUE, FALSE, TRUE)
    )

  data_meta <-
    tibble::tibble(
      dataset_id = 1:2,
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold")
    )

  result <-
    summarise_spd_radius_coverage(data_spd, data_meta)

  table_overall <-
    result |>
    dplyr::filter(.data[["summary_level"]] == "overall")

  testthat::expect_identical(table_overall[["n_datasets"]], c(2L, 2L))
  testthat::expect_identical(table_overall[["n_available"]], c(1L, 2L))
  testthat::expect_identical(
    table_overall[["n_series_available"]],
    c(2L, 2L)
  )
  testthat::expect_true(
    all(c("region", "climatezone") %in% result[["summary_level"]])
  )
})

testthat::test_that("summarise_spd_radius_coverage() rejects duplicate metadata", {
  data_spd <-
    tibble::tibble(
      dataset_id = 1L,
      radius_km = 250L,
      n_time_points = 2L,
      n_finite_values = 2L,
      available = TRUE
    )
  data_meta <-
    tibble::tibble(
      dataset_id = c(1L, 1L),
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold")
    )

  testthat::expect_error(
    summarise_spd_radius_coverage(data_spd, data_meta),
    regexp = "required contract"
  )
})
