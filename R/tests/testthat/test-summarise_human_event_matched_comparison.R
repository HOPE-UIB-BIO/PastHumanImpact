testthat::test_that(
  "summarise_human_event_matched_comparison() validates contrasts",
  {
    testthat::expect_error(
      summarise_human_event_matched_comparison(
        data_comparison = data.frame(),
        metric_cols = "human"
      ),
      "contract"
    )
  }
)
