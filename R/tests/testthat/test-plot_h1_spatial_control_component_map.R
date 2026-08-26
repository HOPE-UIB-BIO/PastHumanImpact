testthat::test_that(
  "component maps use their supplied semantic endpoint colour",
  {
    values <-
      tibble::tibble(
        component = "time",
        long = c(10, 20),
        lat = c(50, 55),
        value = c(-0.2, 0.4)
      )

    result <-
      plot_h1_spatial_control_component_map(
        data_values = values,
        component = "time",
        component_colour = paletete_age[["old"]],
        scale_type = "signed",
        value_limits = c(-0.2, 0.4),
        legend_title = "Unique adjusted R\u00B2"
      )
    colour_scale <- result$scales$get_scales("colour")

    testthat::expect_s3_class(result, "ggplot")
    testthat::expect_identical(
      colour_scale$map(0.4),
      toupper(paletete_age[["old"]])
    )
    testthat::expect_identical(colour_scale$map(0), "#FFFFFF")
  }
)
