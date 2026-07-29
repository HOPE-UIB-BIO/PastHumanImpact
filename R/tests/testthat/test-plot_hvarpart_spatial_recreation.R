testthat::test_that("spatial recreation validates its input contract", {
  testthat::expect_error(
    plot_hvarpart_spatial_recreation(
      data_importance = tibble::tibble(),
      data_meta = tibble::tibble(),
      data_geo_koppen = tibble::tibble()
    ),
    "required contract"
  )
})
