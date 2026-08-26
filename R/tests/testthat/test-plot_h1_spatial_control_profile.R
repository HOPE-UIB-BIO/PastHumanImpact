testthat::test_that(
  "control profiles retain the human-climate-time semantic order",
  {
    values <-
      tidyr::crossing(
        component = c("time", "climate", "human"),
        dataset_id = c("a", "b")
      ) |>
      dplyr::mutate(
        long = dplyr::if_else(
          .data[["dataset_id"]] == "a",
          10,
          20
        ),
        lat = dplyr::if_else(
          .data[["dataset_id"]] == "a",
          50,
          55
        ),
        value = rep(c(0.25, 0.75), 3)
      )
    component_palette <-
      c(
        human = palette_predictors[["human"]],
        climate = palette_predictors[["climate"]],
        time = paletete_age[["old"]]
      )

    result <-
      plot_h1_spatial_control_profile(
        data_values = values,
        component_palette = component_palette,
        scale_type = "allocation",
        legend_title = "Contribution"
    )

    testthat::expect_s3_class(result, "ggplot")
    testthat::expect_identical(
      attr(result, "component_order"),
      c("human", "climate", "time")
    )
    testthat::expect_identical(
      attr(result, "component_palette"),
      component_palette[c("human", "climate", "time")]
    )
  }
)
