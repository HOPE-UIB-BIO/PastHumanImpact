testthat::test_that(
  "plot_h1_spatial_component_distribution() uses one continuous scale",
  {
    values <-
      tibble::tibble(
        dataset_id = 1:8,
        analysis = "spatial_spd",
        region = rep(c("Europe", "Asia"), each = 4),
        climatezone = rep(c("Temperate_no_dry_season", "Arid"), 4),
        long = c(10, 12, 14, 16, 80, 82, 84, 86),
        lat = c(50, 52, 54, 56, 40, 42, 44, 46),
        measure = "untruncated_hierarchical_contribution",
        component = "human",
        value = seq(-0.2, 0.5, length.out = 8)
      )

    koppen <-
      tibble::tibble(
        x = c(0, 1),
        y = c(0, 1),
        climatezone = factor(
          c("Temperate_no_dry_season", "Arid")
        )
      )

    result <-
      plot_h1_spatial_component_distribution(
        data_values = values,
        data_geo_koppen = koppen,
        component_colour = palette_predictors[["human"]],
        value_limits = c(-0.2, 0.5),
        value_breaks = c(-0.2, 0, 0.2, 0.4),
        y_axis_title = paste0(
          "Relative importance\n",
          "(Untruncated signed hierarchical contribution)"
        ),
        legend_title = "Human"
      )

    testthat::expect_named(
      result,
      c(
        "plot",
        "statistical_plot",
        "record_values",
        "climatezone_values",
        "region_values",
        "density_values"
      )
    )
    testthat::expect_s3_class(result[["plot"]], "ggplot")
    testthat::expect_equal(nrow(result[["record_values"]]), 8L)
    testthat::expect_equal(
      result[["climatezone_values"]] |>
        dplyr::slice_min(abs(.data[["value"]]), n = 1) |>
        dplyr::pull(.data[["summary_colour"]]),
      "#F2F2F2"
    )
  }
)
