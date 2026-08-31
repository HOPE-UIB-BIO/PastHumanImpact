testthat::test_that(
  "plot_spatial_thinning_profile() facets thinning distances",
  {
    data_sensitivity <-
      dplyr::bind_rows(
        tibble::tibble(
          sensitivity_type = "unthinned",
          aggregation_level = "overall",
          profile = "signed",
          importance_balance = -0.4,
          distance_km = NA_real_
        ),
        tidyr::crossing(
          sensitivity_type = "thinning",
          aggregation_level = "overall",
          profile = "signed",
          distance_km = c(250, 500),
          repetition = 1:4
        ) |>
          dplyr::mutate(
            importance_balance = seq(-0.6, -0.3, length.out = dplyr::n())
          )
      )

    data_estimates <-
      tibble::tibble(
        aggregation_level = "overall",
        profile = "signed",
        adjusted_balance = -0.2
      )

    res <-
      plot_spatial_thinning_profile(
        data_sensitivity = data_sensitivity,
        data_estimates = data_estimates,
        profile = "signed"
      )

    testthat::expect_s3_class(res, "ggplot")
    testthat::expect_equal(nrow(res[["data"]]), 8L)
    testthat::expect_equal(length(res[["layers"]]), 2L)
    testthat::expect_match(
      res[["labels"]][["x"]],
      "Untruncated signed human-climate hierarchical contribution"
    )
    testthat::expect_match(
      res[["labels"]][["caption"]],
      "one thinned SPD dataset"
    )
  }
)

testthat::test_that(
  "plot_spatial_thinning_profile() validates incomplete inputs",
  {
    testthat::expect_error(
      plot_spatial_thinning_profile(
        data_sensitivity = tibble::tibble(),
        data_estimates = tibble::tibble(),
        profile = "signed"
      ),
      "do not satisfy"
    )

    data_sensitivity <-
      tibble::tibble(
        sensitivity_type = "unthinned",
        aggregation_level = "overall",
        profile = "signed",
        importance_balance = -0.4,
        distance_km = NA_real_
      )

    data_estimates <-
      tibble::tibble(
        aggregation_level = "overall",
        profile = "signed",
        adjusted_balance = -0.2
      )

    testthat::expect_error(
      plot_spatial_thinning_profile(
        data_sensitivity = data_sensitivity,
        data_estimates = data_estimates,
        profile = "signed"
      ),
      "Incomplete thinning"
    )
  }
)

