testthat::test_that("format_core_temporal_caption formats metadata", {
  data_metadata <-
    tibble::tibble(
      long = -70.25,
      lat = 46.4,
      altitude = 1936,
      country = "Canada",
      depositionalenvironment = "Lake",
      doi = "10.1234/example"
    )

  result <-
    format_core_temporal_caption(data_metadata)

  testthat::expect_match(result, "46.4 N, 70.25 W")
  testthat::expect_match(result, "Elevation: 1936 m")
  testthat::expect_match(result, "Country: Canada")
  testthat::expect_match(result, "Environment: Lake")
  testthat::expect_match(result, "DOI: 10.1234/example")
  testthat::expect_match(result, "density metrics")
})

testthat::test_that("format_core_temporal_caption validates metadata", {
  testthat::expect_error(
    format_core_temporal_caption(tibble::tibble(long = 8.5)),
    regexp = "missing caption columns"
  )
})
