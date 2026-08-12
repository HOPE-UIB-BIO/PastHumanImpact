testthat::test_that(
  "classify_spatial_robustness() applies prospective thresholds",
  {
    data_sensitivity <-
      tibble::tibble(
        sensitivity_type = c(
          "baseline",
          rep("thinning", 4),
          "leave_region_out",
          "leave_climatezone_out"
        ),
        aggregation_level = "overall",
        profile = "signed",
        distance_km = c(NA, 250, 250, 500, 500, NA, NA),
        ranking = "climate"
      )
    data_spatial <-
      tibble::tibble(
        aggregation_level = "overall",
        profile = "signed",
        ranking = "climate"
      )

    result <-
      classify_spatial_robustness(
        data_sensitivity = data_sensitivity,
        data_spatial_estimates = data_spatial
      )

    testthat::expect_equal(
      dplyr::pull(result, "robustness_class"),
      "robust"
    )
    testthat::expect_equal(
      dplyr::pull(result, "min_thinning_agreement"),
      1
    )
  }
)

testthat::test_that(
  "classify_spatial_robustness() flags a spatial reversal",
  {
    data_sensitivity <-
      tibble::tibble(
        sensitivity_type = c("baseline", "thinning"),
        aggregation_level = "overall",
        profile = "signed",
        distance_km = c(NA, 250),
        ranking = "climate"
      )
    data_spatial <-
      tibble::tibble(
        aggregation_level = "overall",
        profile = "signed",
        ranking = "human"
      )

    result <-
      classify_spatial_robustness(
        data_sensitivity = data_sensitivity,
        data_spatial_estimates = data_spatial
      )

    testthat::expect_equal(
      dplyr::pull(result, "robustness_class"),
      "spatially_sensitive"
    )
  }
)
