testthat::test_that(
  "diagnose_human_event_summary_reconciliation() validates inputs",
  {
    testthat::expect_error(
      diagnose_human_event_summary_reconciliation(
        data.frame(),
        data.frame()
      ),
      "contract"
    )
  }
)
