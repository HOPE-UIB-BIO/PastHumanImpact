testthat::test_that(
  "plot_h1_temporal_control_profiles() separates profile calculations",
  {
    data_components <-
      tidyr::crossing(
        analysis = c("temporal_spd", "temporal_events"),
        region = c("North America", "Europe"),
        age = c(2000, 2500),
        predictor = c("human", "climate", "space")
      ) |>
      dplyr::mutate(
        model_profile = "human_climate_space",
        individual = seq(0.01, 0.24, length.out = dplyr::n())
      )

    data_unique_adjusted_r2 <-
      tidyr::crossing(
        analysis = c("temporal_spd", "temporal_events"),
        region = c("North America", "Europe"),
        age = c(2000, 2500),
        fraction = c("pure_human", "pure_climate", "pure_space")
      ) |>
      dplyr::mutate(
        adjusted_r_squared = seq(-0.02, 0.21, length.out = dplyr::n())
      )

    res <-
      plot_h1_temporal_control_profiles(
        data_components = data_components,
        data_unique_adjusted_r2 = data_unique_adjusted_r2
      )

    testthat::expect_named(
      res,
      c(
        "untruncated_hierarchical_contributions",
        "unique_adjusted_r2"
      )
    )

    testthat::expect_true(
      purrr::every(res, ~ inherits(.x, "ggplot"))
    )

    data_unique <-
      res[["unique_adjusted_r2"]][["data"]]

    testthat::expect_identical(
      levels(data_unique[["analysis_label"]]),
      c("SPD", "Events")
    )

    testthat::expect_identical(
      levels(data_unique[["region_label"]]),
      unname(region_labeller)
    )
  }
)

testthat::test_that(
  "plot_h1_temporal_control_profiles() validates required columns",
  {
    testthat::expect_error(
      plot_h1_temporal_control_profiles(
        data_components = tibble::tibble(),
        data_unique_adjusted_r2 = tibble::tibble()
      ),
      "inputs are invalid"
    )
  }
)
