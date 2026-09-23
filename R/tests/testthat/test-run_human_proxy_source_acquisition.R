testthat::test_that("run_human_proxy_source_acquisition() validates manifest", {
  testthat::expect_error(
    run_human_proxy_source_acquisition(
      data_sources = tibble::tibble(source_id = "kk10"),
      data_storage_path = tempdir(),
      validate_rasters = FALSE
    ),
    regexp = "do not satisfy"
  )
})
