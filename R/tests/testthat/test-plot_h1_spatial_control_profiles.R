testthat::test_that(
  "plot_h1_spatial_control_profiles() creates three map profiles",
  {
    records <-
      tibble::tibble(
        dataset_id = "a",
        analysis = "spatial_spd",
        long = 10,
        lat = 50,
        human_allocation = 0.4,
        climate_allocation = 0.4,
        time_allocation = 0.2
      )
    components <-
      tibble::tibble(
        dataset_id = "a",
        analysis = "spatial_spd",
        model_profile = "human_climate_time",
        predictor = c("human", "climate", "time"),
        individual = c(0.2, 0.3, -0.1)
      )
    partial <-
      tibble::tibble(
        dataset_id = "a",
        analysis = "spatial_spd",
        fraction = c("pure_human", "pure_climate", "pure_time"),
        adjusted_r_squared = c(0.1, 0.2, -0.1)
      )
    result <-
      plot_h1_spatial_control_profiles(records, components, partial)

    testthat::expect_named(
      result,
      c(
        "zero_truncated_hierarchical_composition",
        "untruncated_hierarchical_contributions",
        "unique_adjusted_r2"
      )
    )
    testthat::expect_true(purrr::every(result, ~ inherits(.x, "ggplot")))
  }
)
