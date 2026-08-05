testthat::test_that(
  "plot_hvarpart_adjr2_distribution() validates its input contract",
  {
    testthat::expect_error(
      plot_hvarpart_adjr2_distribution(
        data_decomposition = tibble::tibble(),
        data_meta = tibble::tibble(),
        data_geo_koppen = tibble::tibble()
      ),
      "distribution contract"
    )
  }
)
