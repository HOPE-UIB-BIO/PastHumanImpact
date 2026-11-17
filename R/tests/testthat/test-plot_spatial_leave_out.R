testthat::test_that(
  "plot_spatial_leave_out() keeps overall leave-out rows with labels",
  {
    data_sensitivity <-
      tibble::tibble(
        sensitivity_type = c(
          "unthinned",
          "thinning",
          "leave_region_out",
          "leave_climatezone_out",
          "leave_region_out"
        ),
        aggregation_level = c(
          "overall",
          "overall",
          "overall",
          "overall",
          "region"
        ),
        profile = c(
          "signed",
          "signed",
          "signed",
          "zero_truncated",
          "signed"
        ),
        importance_balance = c(-0.4, -0.3, -0.2, -0.1, 0.5),
        distance_km = c(NA, 250, NA, NA, NA),
        omitted_group = c(
          NA,
          NA,
          "Latin America",
          "Temperate",
          "Europe"
        )
      )

    result <-
      plot_spatial_leave_out(
        data_sensitivity = data_sensitivity
      )

    testthat::expect_s3_class(result, "ggplot")

    data_leave_out <-
      result[["data"]]

    testthat::expect_equal(nrow(data_leave_out), 2L)

    testthat::expect_setequal(
      as.character(data_leave_out[["omitted_group_label"]]),
      c("Central & South America", "Temperate")
    )

    testthat::expect_setequal(
      as.character(data_leave_out[["sensitivity_label"]]),
      c("Continental group omitted", "Region omitted")
    )

    testthat::expect_identical(
      levels(data_leave_out[["omitted_group_label"]]),
      rev(c(
        unname(region_labeller),
        data_climate_zones[["climatezone_label"]]
      ))
    )

    testthat::expect_identical(
      levels(data_leave_out[["profile_label"]]),
      c(
        "Untruncated signed contribution",
        "Zero-truncated importance balance"
      )
    )

    testthat::expect_equal(
      unname(
        result[["scales"]]$get_scales("shape")$palette(2)
      ),
      c(21, 22)
    )
  }
)

testthat::test_that(
  "plot_spatial_leave_out() validates the sensitivity table",
  {
    testthat::expect_error(
      plot_spatial_leave_out(
        data_sensitivity = tibble::tibble()
      ),
      "do not satisfy"
    )
  }
)
