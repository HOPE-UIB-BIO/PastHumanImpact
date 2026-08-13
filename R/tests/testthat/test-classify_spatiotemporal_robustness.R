testthat::test_that(
  "classify_spatiotemporal_robustness() applies declared thresholds",
  {
    data_human_climate_only <-
      tibble::tibble(
        sensitivity_type = "human_climate_only",
        aggregation_level = "overall",
        profile = "zero_truncated",
        ranking = "human"
      )
    data_sensitivity <-
      dplyr::bind_rows(
        tibble::tibble(
          sensitivity_type = "unthinned",
          aggregation_level = "overall",
          profile = "zero_truncated",
          ranking = "human",
          distance_km = NA_real_
        ),
        tibble::tibble(
          sensitivity_type = "thinning",
          aggregation_level = "overall",
          profile = "zero_truncated",
          ranking = rep("human", 20),
          distance_km = rep(c(250, 500), each = 10)
        ),
        tibble::tibble(
          sensitivity_type = "leave_region_out",
          aggregation_level = "overall",
          profile = "zero_truncated",
          ranking = "human",
          distance_km = NA_real_
        )
      )
    data_spatial <-
      tibble::tibble(
        aggregation_level = "overall",
        profile = "zero_truncated",
        ranking = "human"
      )
    result <-
      classify_spatiotemporal_robustness(
        data_sensitivity = data_sensitivity,
        data_spatial_estimates = data_spatial,
        data_human_climate_only = data_human_climate_only
      )

    testthat::expect_equal(result$robustness_class, "robust")
    testthat::expect_equal(result$min_thinning_agreement, 1)
  }
)
