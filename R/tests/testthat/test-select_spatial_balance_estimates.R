testthat::test_that(
  "select_spatial_balance_estimates() keeps one requested profile",
  {
    data_estimates <-
      tidyr::crossing(
        aggregation_level = "region_climatezone",
        profile = c("zero_truncated", "signed"),
        region = c("Europe", "Asia"),
        climatezone = "Polar"
      ) |>
      dplyr::mutate(
        adjusted_balance = dplyr::if_else(
          .data[["profile"]] == "zero_truncated",
          -0.5,
          0.5
        )
      )

    result <-
      select_spatial_balance_estimates(
        data_estimates = data_estimates,
        aggregation_level = "region_climatezone",
        profile = "zero_truncated"
      )

    testthat::expect_equal(nrow(result), 2L)
    testthat::expect_true(
      all(dplyr::pull(result, profile) == "zero_truncated")
    )
    testthat::expect_equal(
      dplyr::pull(result, adjusted_balance),
      c(-0.5, -0.5)
    )
  }
)

testthat::test_that(
  "select_spatial_balance_estimates() rejects duplicate plotted groups",
  {
    data_estimates <-
      tibble::tibble(
        aggregation_level = "region_climatezone",
        profile = "signed",
        region = "Europe",
        climatezone = "Polar",
        adjusted_balance = c(-0.2, 0.1)
      )

    testthat::expect_error(
      select_spatial_balance_estimates(
        data_estimates = data_estimates,
        aggregation_level = "region_climatezone",
        profile = "signed"
      ),
      "must be unique"
    )
  }
)

testthat::test_that(
  "select_spatial_balance_estimates() validates its input",
  {
    testthat::expect_error(
      select_spatial_balance_estimates(
        data_estimates = tibble::tibble(),
        aggregation_level = "region",
        profile = "signed"
      ),
      "does not satisfy"
    )
  }
)
