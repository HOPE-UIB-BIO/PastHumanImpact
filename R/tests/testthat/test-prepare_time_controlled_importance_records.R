testthat::test_that(
  "prepare_time_controlled_importance_records() calculates bounded balance",
  {
    controlled_components <-
      tidyr::expand_grid(
        dataset_id = "a",
        analysis = "spatial_spd",
        predictor = c("human", "climate", "time")
      ) |>
      dplyr::mutate(
        individual = c(0.2, 0.6, 0.2),
        model_profile = "human_climate_time",
        total_adjusted_r_squared = 1
      )
    human_climate_only_components <-
      tidyr::expand_grid(
        dataset_id = "a",
        analysis = "spatial_spd",
        predictor = c("human", "climate")
      ) |>
      dplyr::mutate(
        individual = c(0.7, 0.3),
        model_profile = "human_climate",
        total_adjusted_r_squared = 1
      )
    components <-
      dplyr::bind_rows(
        controlled_components,
        human_climate_only_components
      )
    status <-
      tibble::tibble(
        dataset_id = "a",
        analysis = "spatial_spd",
        status = "estimated"
      )
    metadata <-
      tibble::tibble(
        dataset_id = "a",
        long = 10,
        lat = 50,
        region = "Europe",
        climatezone = "Temperate"
      )
    result <-
      prepare_time_controlled_importance_records(
        data_components = components,
        data_status = status,
        data_meta = metadata
      )

    testthat::expect_equal(result$zero_balance, -0.5)
    testthat::expect_equal(result$human_climate_only_zero_balance, 0.4)
    testthat::expect_equal(
      result$human_allocation + result$climate_allocation +
        result$time_allocation,
      1
    )
  }
)
