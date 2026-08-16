testthat::test_that(
  "plot_h1_temporal_control_profiles() separates profile calculations",
  {
    components <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        model_profile = "human_climate_space",
        predictor = c("human", "climate", "space"),
        individual = c(0.4, 0.3, -0.1)
      )
    partial <-
      tibble::tibble(
        analysis = "temporal_spd",
        region = "Europe",
        age = 2000,
        fraction = c("pure_human", "pure_climate", "pure_space"),
        adjusted_r_squared = c(0.2, 0.1, -0.1)
      )
    result <-
      plot_h1_temporal_control_profiles(components, partial)

    testthat::expect_named(
      result,
      c(
        "untruncated_hierarchical_contributions",
        "unique_adjusted_r2"
      )
    )
    testthat::expect_true(purrr::every(result, ~ inherits(.x, "ggplot")))
  }
)
