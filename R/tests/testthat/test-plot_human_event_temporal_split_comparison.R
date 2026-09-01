testthat::test_that(
  "plot_human_event_temporal_split_comparison() validates inputs",
  {
    testthat::expect_error(
      plot_human_event_temporal_split_comparison(
        data.frame(),
        data.frame(),
        data.frame()
      ),
      "contract"
    )
  }
)