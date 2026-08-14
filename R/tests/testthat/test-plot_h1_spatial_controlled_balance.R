testthat::test_that(
  "plot_h1_spatial_controlled_balance() uses the canonical layout",
  {
    records <-
      tibble::tibble(
        dataset_id = 1:8,
        analysis = rep("spatial_spd", 8),
        region = rep(c("Europe", "Asia"), each = 4),
        climatezone = rep(c("Polar", "Arid"), 4),
        long = c(10, 12, 14, 16, 80, 82, 84, 86),
        lat = c(50, 52, 54, 56, 40, 42, 44, 46),
        zero_balance = seq(-0.8, 0.6, length.out = 8)
      )
    estimates <-
      dplyr::bind_rows(
        tibble::tibble(
          aggregation_level = "region",
          profile = "zero_truncated",
          region = c("Europe", "Asia"),
          climatezone = "All",
          adjusted_balance = c(-0.2, 0.1)
        ),
        tibble::tibble(
          aggregation_level = "region_climatezone",
          profile = "zero_truncated",
          region = rep(c("Europe", "Asia"), each = 2),
          climatezone = rep(c("Polar", "Arid"), 2),
          adjusted_balance = c(-0.4, 0.1, -0.2, 0.3)
        )
      )
    koppen <-
      tibble::tibble(
        x = c(0, 1),
        y = c(0, 1),
        climatezone = factor(c("Polar", "Arid"))
      )
    result <-
      plot_h1_spatial_controlled_balance(
        data_records = records,
        data_estimates = estimates,
        data_geo_koppen = koppen
      )

    testthat::expect_s3_class(result, "ggplot")
    testthat::expect_true(length(result[["layers"]]) > 0L)
  }
)

testthat::test_that(
  "plot_h1_spatial_controlled_balance() validates map data",
  {
    testthat::expect_error(
      plot_h1_spatial_controlled_balance(
        data_records = tibble::tibble(),
        data_estimates = tibble::tibble(),
        data_geo_koppen = tibble::tibble()
      ),
      "do not satisfy"
    )
  }
)
