testthat::test_that(
  "fit_human_proxy_spatial_decision_metric() reports three profiles",
  {
    set.seed(900723)

    n_records <- 40L

    data_records <-
      tibble::tibble(
        model_id = stringr::str_c("model_", base::seq_len(n_records)),
        long = base::seq(0, 3.9, by = 0.1),
        lat = 45,
        region = base::rep(c("A", "B"), each = 20),
        climatezone = base::rep(c("cold", "warm"), times = 20),
        human = stats::runif(n_records, min = -0.05, max = 0.2),
        climate = stats::runif(n_records, min = -0.05, max = 0.3),
        time = stats::runif(n_records, min = -0.05, max = 0.2)
      ) |>
      dplyr::mutate(
        total_adjusted_r_squared =
          .data[["human"]] + .data[["climate"]] + .data[["time"]],
        signed_difference = .data[["human"]] - .data[["climate"]],
        signed_weight = base::abs(.data[["total_adjusted_r_squared"]]),
        zero_weight =
          base::pmax(.data[["human"]], 0) + base::pmax(.data[["climate"]], 0),
        zero_balance = (
          base::pmax(.data[["human"]], 0) - base::pmax(.data[["climate"]], 0)
        ) / .data[["zero_weight"]]
      ) |>
      dplyr::filter(
        .data[["zero_weight"]] > 0,
        .data[["signed_weight"]] > 0
      )

    n_estimable <-
      base::nrow(data_records)

    res <-
      fit_human_proxy_spatial_decision_metric(
        data_records = data_records,
        n_bootstrap = 99L,
        seed = 900723L,
        permutations = 19L,
        min_unique_locations = 10L,
        min_residual_df = 5L,
        distance_km = 50
      )

    testthat::expect_type(res, "list")
    testthat::expect_true(
      base::all(
        c("metric", "estimates", "moran_diagnostics") %in% base::names(res)
      )
    )

    data_metric <-
      res[["metric"]]

    testthat::expect_named(
      data_metric,
      c(
        "profile", "adjusted_balance", "lwr_95", "upr_95", "n_records",
        "n_human_dominant", "n_bootstrap", "n_failed_replicates",
        "fit_status", "error_message"
      )
    )
    testthat::expect_setequal(
      dplyr::pull(data_metric, "profile"),
      c("signed", "zero_truncated", "exclude_negative")
    )

    n_non_negative <-
      data_records |>
      dplyr::filter(
        .data[["human"]] >= 0,
        .data[["climate"]] >= 0,
        .data[["time"]] >= 0
      ) |>
      base::nrow()

    data_exclude <-
      data_metric |>
      dplyr::filter(.data[["profile"]] == "exclude_negative")

    testthat::expect_equal(
      dplyr::pull(data_exclude, "n_records"),
      n_non_negative
    )

    data_zero <-
      data_metric |>
      dplyr::filter(.data[["profile"]] == "zero_truncated")

    testthat::expect_equal(dplyr::pull(data_zero, "n_records"), n_estimable)
    testthat::expect_equal(
      dplyr::pull(data_zero, "n_human_dominant"),
      base::sum(dplyr::pull(data_records, "zero_balance") > 0)
    )

    value_estimate <-
      res[["estimates"]] |>
      dplyr::filter(
        .data[["aggregation_level"]] == "overall",
        .data[["profile"]] == "zero_truncated"
      ) |>
      dplyr::pull("adjusted_balance")

    testthat::expect_equal(
      dplyr::pull(data_zero, "adjusted_balance"),
      value_estimate
    )
    testthat::expect_true(
      base::all(dplyr::pull(data_metric, "fit_status") == "estimated")
    )
    testthat::expect_true(
      "exclude_negative" %in% dplyr::pull(res[["estimates"]], "profile")
    )
  }
)

testthat::test_that(
  "fit_human_proxy_spatial_decision_metric() validates its inputs",
  {
    testthat::expect_error(
      fit_human_proxy_spatial_decision_metric(
        data_records = tibble::tibble(model_id = "a")
      ),
      regexp = "decision-metric records"
    )

    data_records <-
      tibble::tibble(
        model_id = c("a", "b"),
        long = c(0, 1),
        lat = 45,
        region = "A",
        climatezone = "cold",
        signed_difference = c(0.1, -0.1),
        signed_weight = c(0.2, 0.3),
        zero_balance = c(0.5, NA),
        zero_weight = c(0.2, 0),
        human = c(0.1, -0.1),
        climate = c(0, -0.1),
        time = 0.1
      )

    testthat::expect_error(
      fit_human_proxy_spatial_decision_metric(data_records = data_records),
      regexp = "decision-metric records"
    )
  }
)
