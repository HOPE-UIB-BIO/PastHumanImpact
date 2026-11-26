testthat::test_that(
  "summarise_human_proxy_temporal_decision_metric() returns three profiles",
  {
    set.seed(900723)

    data_units <-
      tidyr::expand_grid(
        region = c("A", "B"),
        age = base::seq(2000, 5000, by = 500)
      ) |>
      dplyr::mutate(
        human = stats::runif(dplyr::n(), min = -0.02, max = 0.2),
        climate = stats::runif(dplyr::n(), min = 0, max = 0.3),
        space = stats::runif(dplyr::n(), min = 0, max = 0.1)
      )

    data_composition <-
      data_units |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(c("human", "climate", "space")),
        names_to = "predictor",
        values_to = "individual"
      ) |>
      dplyr::mutate(status = "spatial_model_estimated")

    res <-
      summarise_human_proxy_temporal_decision_metric(
        data_composition = data_composition,
        block_length = 3L,
        n_bootstrap = 99L,
        seed = 900723L
      )

    testthat::expect_named(res, c("metric", "units"))
    testthat::expect_named(
      res[["metric"]],
      c(
        "profile", "pooled_difference", "lwr_95", "upr_95",
        "mean_difference", "n_units", "n_human_greater", "n_bootstrap",
        "n_failed_replicates"
      )
    )
    testthat::expect_equal(
      dplyr::pull(res[["metric"]], "profile"),
      c("zero_truncated", "signed", "exclude_negative")
    )

    data_expected <-
      compute_pooled_human_climate_allocation(
        data_units = data_units,
        profile = c("zero_truncated", "signed", "exclude_negative")
      )

    testthat::expect_equal(
      dplyr::pull(res[["metric"]], "pooled_difference"),
      dplyr::pull(data_expected, "pooled_difference")
    )
    testthat::expect_equal(base::nrow(res[["units"]]), base::nrow(data_units))
  }
)

testthat::test_that(
  "summarise_human_proxy_temporal_decision_metric() needs complete units",
  {
    data_composition <-
      tibble::tibble(
        region = "A",
        age = 2000,
        predictor = c("human", "climate"),
        individual = c(0.1, 0.2)
      )

    testthat::expect_error(
      summarise_human_proxy_temporal_decision_metric(
        data_composition = data_composition,
        n_bootstrap = 9L
      ),
      regexp = "human, climate, and space"
    )

    testthat::expect_error(
      summarise_human_proxy_temporal_decision_metric(
        data_composition = tibble::tibble(region = "A")
      ),
      regexp = "Temporal decision-metric inputs"
    )
  }
)
