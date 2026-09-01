testthat::test_that(
  "prepare_human_event_h1_results() validates branch results",
  {
    testthat::expect_error(
      prepare_human_event_h1_results(list(), "status"),
      "contract"
    )
  }
)
