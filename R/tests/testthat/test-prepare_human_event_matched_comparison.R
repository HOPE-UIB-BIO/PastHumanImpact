testthat::test_that(
  "prepare_human_event_matched_comparison() computes both SPD contrasts",
  {
    source <-
      tidyr::crossing(
        cohort = "as_coded",
        dataset_id = c("a", "b"),
        proxy_variant = c("spd", "spd_events", "events")
      ) |>
      dplyr::mutate(
        status = "estimated",
        human = c(spd = 1, spd_events = 3, events = 0)[
          .data[["proxy_variant"]]
        ],
        ranking = dplyr::if_else(.data[["human"]] > 0, "human", "climate")
      )

    result <- prepare_human_event_matched_comparison(
      data_source = source,
      key_cols = "dataset_id",
      metric_cols = "human",
      ranking_cols = "ranking",
      estimable_statuses = "estimated"
    )

    testthat::expect_true(all(result[["human__spd_events_minus_spd"]] == 2))
    testthat::expect_true(all(result[["human__events_minus_spd"]] == -1))
    testthat::expect_true(all(result[["three_way_estimable"]]))
    testthat::expect_true(
      all(result[["ranking__events_vs_spd_reversal"]])
    )
  }
)

testthat::test_that(
  "human-event summaries never mix chronology cohorts",
  {
    comparison <- tibble::tibble(
      cohort = c("as_coded", "observed_events_only"),
      human__spd_events_minus_spd = c(1, 10),
      human__events_minus_spd = c(-1, -10)
    )

    result <- summarise_human_event_matched_comparison(
      data_comparison = comparison,
      metric_cols = "human"
    )

    testthat::expect_equal(nrow(result), 4L)
    testthat::expect_equal(
      sort(result[["median"]]),
      c(-10, -1, 1, 10)
    )
    reconciliation <- diagnose_human_event_summary_reconciliation(
      data_comparison = comparison,
      data_summary = result
    )
    testthat::expect_true(all(reconciliation[["reconciled"]]))
  }
)

testthat::test_that(
  "human-event summaries use only three-way-estimable units",
  {
    comparison <- tibble::tibble(
      cohort = rep("as_coded", 2),
      three_way_estimable = c(TRUE, FALSE),
      human__spd_events_minus_spd = c(1, 100),
      human__events_minus_spd = c(-1, -100)
    )

    result <- summarise_human_event_matched_comparison(
      data_comparison = comparison,
      metric_cols = "human"
    )

    testthat::expect_equal(result[["n_units"]], c(1L, 1L))
    testthat::expect_equal(sort(result[["median"]]), c(-1, 1))
    reconciliation <- diagnose_human_event_summary_reconciliation(
      data_comparison = comparison,
      data_summary = result
    )
    testthat::expect_true(all(reconciliation[["reconciled"]]))
  }
)
