testthat::test_that(
  "prepare_h1_spatial_control_values() joins reciprocal control values",
  {
    records <-
      tibble::tibble(
        dataset_id = c("a", "b"),
        analysis = "spatial_spd",
        region = c("Europe", "Asia"),
        climatezone = c("Temperate_no_dry_season", "Arid"),
        long = c(10, 80),
        lat = c(50, 40)
      )

    components <-
      tidyr::crossing(
        dataset_id = c("a", "b"),
        predictor = c("human", "climate", "time")
      ) |>
      dplyr::mutate(
        analysis = "spatial_spd",
        model_profile = "human_climate_time",
        individual = seq(0.1, 0.6, length.out = dplyr::n())
      )

    fractions <-
      tidyr::crossing(
        dataset_id = c("a", "b"),
        fraction = c("pure_human", "pure_climate", "pure_time")
      ) |>
      dplyr::mutate(
        analysis = "spatial_spd",
        adjusted_r_squared = seq(-0.1, 0.4, length.out = dplyr::n())
      )

    result <-
      prepare_h1_spatial_control_values(
        data_records = records,
        data_components = components,
        data_unique_adjusted_r2 = fractions
      )

    testthat::expect_equal(nrow(result), 12L)
    testthat::expect_setequal(
      result[["measure"]],
      c(
        "untruncated_hierarchical_contribution",
        "unique_adjusted_r2"
      )
    )
    testthat::expect_setequal(
      result[["component"]],
      c("human", "climate", "time")
    )
  }
)

testthat::test_that(
  "prepare_h1_spatial_control_values() rejects conflicting metadata",
  {
    records <-
      tibble::tibble(
        dataset_id = c("a", "a"),
        analysis = "spatial_spd",
        region = "Europe",
        climatezone = "Temperate_no_dry_season",
        long = c(10, 11),
        lat = 50
      )

    testthat::expect_error(
      prepare_h1_spatial_control_values(
        data_records = records,
        data_components = tibble::tibble(
          dataset_id = "a",
          analysis = "spatial_spd",
          model_profile = "human_climate_time",
          predictor = "human",
          individual = 0.1
        ),
        data_unique_adjusted_r2 = tibble::tibble(
          dataset_id = "a",
          analysis = "spatial_spd",
          fraction = "pure_human",
          adjusted_r_squared = 0.1
        )
      ),
      "one spatial record"
    )
  }
)
