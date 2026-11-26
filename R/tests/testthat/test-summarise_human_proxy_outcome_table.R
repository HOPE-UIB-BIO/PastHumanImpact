testthat::test_that(
  "summarise_human_proxy_outcome_table() builds the outcome table",
  {
    data_metrics <-
      tidyr::expand_grid(
        analysis = "spatial_h1",
        comparison = "main",
        human_proxy = c("spd", "kk10"),
        proxy_support = "spd_buffer",
        age_window = c("common_cohort", "native"),
        profile = c("zero_truncated", "signed", "exclude_negative")
      ) |>
      dplyr::mutate(
        estimate = -0.2,
        lwr_95 = -0.3,
        upr_95 = -0.1,
        n_units = 100L
      ) |>
      dplyr::mutate(
        upr_95 = dplyr::if_else(
          .data[["human_proxy"]] == "kk10" &
            .data[["age_window"]] == "native" &
            .data[["profile"]] == "signed",
          0.1,
          .data[["upr_95"]]
        )
      )

    res <-
      summarise_human_proxy_outcome_table(data_metrics = data_metrics)

    testthat::expect_named(res, c("long", "summary", "table"))
    testthat::expect_true("outcome_class" %in% base::names(res[["long"]]))
    testthat::expect_equal(base::nrow(res[["summary"]]), 4L)
    testthat::expect_true(
      base::all(
        c(
          "outcome_class", "outcome_class_signed",
          "outcome_class_exclude_negative", "class_changes_between_profiles"
        ) %in% base::names(res[["summary"]])
      )
    )

    data_changed <-
      res[["summary"]] |>
      dplyr::filter(.data[["class_changes_between_profiles"]])

    testthat::expect_equal(base::nrow(data_changed), 1L)
    testthat::expect_equal(dplyr::pull(data_changed, "human_proxy"), "kk10")
    testthat::expect_equal(
      dplyr::pull(data_changed, "outcome_class"),
      "climate"
    )

    data_table <-
      res[["table"]]

    testthat::expect_equal(base::nrow(data_table), 2L)
    testthat::expect_true(
      base::all(c("common_cohort", "native") %in% base::names(data_table))
    )
    testthat::expect_true(
      base::all(dplyr::pull(data_table, "all_cells_same_class"))
    )
  }
)

testthat::test_that(
  "summarise_human_proxy_outcome_table() detects differing cells",
  {
    data_metrics <-
      tibble::tibble(
        analysis = "temporal_h1",
        comparison = "main",
        human_proxy = c("spd", "kk10"),
        proxy_support = "spd_buffer",
        age_window = "common_cohort",
        profile = "zero_truncated",
        estimate = c(-0.2, 0.2),
        lwr_95 = c(-0.3, 0.1),
        upr_95 = c(-0.1, 0.3),
        n_units = 50L
      )

    res <-
      summarise_human_proxy_outcome_table(data_metrics = data_metrics)

    testthat::expect_false(
      base::any(dplyr::pull(res[["table"]], "all_cells_same_class"))
    )
  }
)

testthat::test_that(
  "summarise_human_proxy_outcome_table() pivots several factors",
  {
    data_metrics <-
      tidyr::expand_grid(
        analysis = "spatial_h1",
        comparison = "main_proxies",
        human_proxy = c("spd", "kk10"),
        gridded_support = c("spd_buffer", "radius_50km"),
        age_window = c("common_cohort", "native"),
        record_set = c("shared_records", "all_records"),
        profile = c("zero_truncated", "signed")
      ) |>
      dplyr::mutate(
        proxy_support = dplyr::if_else(
          .data[["human_proxy"]] == "spd",
          "spd_buffer",
          .data[["gridded_support"]]
        ),
        estimate = 0.1,
        lwr_95 = 0.05,
        upr_95 = 0.2,
        n_units = 10L
      )

    res <-
      summarise_human_proxy_outcome_table(
        data_metrics = data_metrics,
        row_columns = c("analysis", "comparison", "human_proxy"),
        table_columns = c("gridded_support", "age_window", "record_set"),
        group_columns = c("analysis", "comparison")
      )

    testthat::expect_equal(base::nrow(res[["summary"]]), 16L)
    testthat::expect_true(
      "proxy_support" %in% base::names(res[["summary"]])
    )

    data_table <-
      res[["table"]]

    testthat::expect_equal(base::nrow(data_table), 2L)
    testthat::expect_true(
      base::all(
        c(
          "spd_buffer__common_cohort__shared_records",
          "radius_50km__native__all_records"
        ) %in% base::names(data_table)
      )
    )
    testthat::expect_equal(
      base::ncol(data_table),
      3L + 8L + 1L
    )
    testthat::expect_true(
      base::all(dplyr::pull(data_table, "all_cells_same_class"))
    )
  }
)

testthat::test_that(
  "summarise_human_proxy_outcome_table() validates its inputs",
  {
    testthat::expect_error(
      summarise_human_proxy_outcome_table(
        data_metrics = tibble::tibble(analysis = "spatial_h1")
      ),
      regexp = "Outcome-table inputs"
    )
  }
)
