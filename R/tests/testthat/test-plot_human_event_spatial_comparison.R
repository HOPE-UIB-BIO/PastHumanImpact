testthat::test_that(
  "plot_human_event_spatial_comparison() validates inputs",
  {
    testthat::expect_error(
      plot_human_event_spatial_comparison(data.frame(), data.frame()),
      "contract"
    )
  }
)
