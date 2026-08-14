testthat::test_that(
  "plot_spatial_sensitivity_results() returns all supplementary plots",
  {
    data_moran <-
      tidyr::crossing(
        distance_km = c(250, 500),
        stage = c("unfiltered", "residual"),
        profile = c("signed", "zero_truncated")
      ) |>
      dplyr::mutate(
        moran_i = seq(-0.1, 0.1, length.out = dplyr::n()),
        spatial_scope = "global"
      )
    data_sensitivity <-
      tibble::tibble(
        sensitivity_type = c(
          "unthinned",
          "thinning",
          "leave_region_out",
          "leave_climatezone_out"
        ),
        aggregation_level = "overall",
        profile = "signed",
        importance_balance = c(-0.4, -0.3, -0.2, -0.1),
        distance_km = c(NA, 250, NA, NA),
        omitted_group = c(NA, NA, "Europe", "cold")
      )
    data_estimates <-
      tibble::tibble(
        aggregation_level = "overall",
        profile = "signed",
        adjusted_balance = -0.35
      )
    data_components <-
      tidyr::crossing(
        region = "Europe",
        age = c(2000, 2500),
        model_profile = c("human_climate", "human_climate_space"),
        predictor = c("human", "climate")
      ) |>
      dplyr::mutate(Individual = seq(0.1, 0.8, length.out = dplyr::n()))
    data_unique_adjusted_r2 <-
      tidyr::crossing(
        region = "Europe",
        age = c(2000, 2500),
        fraction = c("pure_human", "pure_climate", "pure_space")
      ) |>
      dplyr::mutate(adjusted_r_squared = 0.1)

    result <-
      plot_spatial_sensitivity_results(
        data_moran = data_moran,
        data_sensitivity = data_sensitivity,
        data_estimates = data_estimates,
        data_temporal_components = data_components,
        data_unique_adjusted_r2 = data_unique_adjusted_r2
      )

    testthat::expect_named(
      result,
      c(
        "human_climate_balance_moran",
        "human_climate_balance_spatial_thinning",
        "human_climate_balance_leave_out",
        "time_control_hierarchical_contributions",
        "time_control_unique_adjusted_r2"
      )
    )
    testthat::expect_true(
      all(purrr::map_lgl(result, ~ inherits(.x, "ggplot")))
    )
  }
)

testthat::test_that(
  "plot_spatial_sensitivity_results() validates table contracts",
  {
    testthat::expect_error(
      plot_spatial_sensitivity_results(
        data_moran = tibble::tibble(),
        data_sensitivity = tibble::tibble(),
        data_estimates = tibble::tibble(),
        data_temporal_components = tibble::tibble(),
        data_unique_adjusted_r2 = tibble::tibble()
      ),
      "do not satisfy"
    )
  }
)
