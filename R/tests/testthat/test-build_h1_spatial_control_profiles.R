testthat::test_that(
  "build_h1_spatial_control_profiles() builds six one-variable profiles",
  {
    records <-
      tibble::tibble(
        dataset_id = rep(letters[1:8], 1),
        analysis = "spatial_spd",
        region = rep(c("Europe", "Asia"), each = 4),
        climatezone = rep(c("Temperate_no_dry_season", "Arid"), 4),
        long = c(10, 12, 14, 16, 80, 82, 84, 86),
        lat = c(50, 52, 54, 56, 40, 42, 44, 46)
      )

    components <-
      tidyr::crossing(
        dataset_id = letters[1:8],
        predictor = c("human", "climate", "time")
      ) |>
      dplyr::mutate(
        analysis = "spatial_spd",
        model_profile = "human_climate_time",
        individual = seq(-0.2, 0.6, length.out = dplyr::n())
      )

    fractions <-
      tidyr::crossing(
        dataset_id = letters[1:8],
        fraction = c("pure_human", "pure_climate", "pure_time")
      ) |>
      dplyr::mutate(
        analysis = "spatial_spd",
        adjusted_r_squared = seq(-0.3, 0.7, length.out = dplyr::n())
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
      build_h1_spatial_control_profiles(
        data_records = records,
        data_components = components,
        data_unique_adjusted_r2 = fractions,
        data_geo_koppen = koppen
      )

    testthat::expect_named(
      result,
      c(
        "climate_unique_adjusted_r2",
        "human_unique_adjusted_r2",
        "time_unique_adjusted_r2",
        "climate_untruncated_hierarchical_contribution",
        "human_untruncated_hierarchical_contribution",
        "time_untruncated_hierarchical_contribution"
      )
    )
    testthat::expect_true(
      purrr::every(result, ~ inherits(.x[["plot"]], "ggplot"))
    )
  }
)
