testthat::test_that(
  "build_human_event_evidence_tables() rejects empty inputs",
  {
    testthat::expect_error(
      build_human_event_evidence_tables(
        data.frame(),
        data.frame(),
        data.frame(),
        data.frame(),
        data.frame(),
        data.frame(),
        data.frame(),
        data.frame()
      )
    )
  }
)
