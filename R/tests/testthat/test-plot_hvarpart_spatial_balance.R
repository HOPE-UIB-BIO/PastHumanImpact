testthat::test_that(
  "plot_hvarpart_spatial_balance() creates the bounded balance",
  {
    data_importance <-
      tibble::tibble(
        analysis = "spatial_spd",
        model_id = rep(c("one", "two", "three", "four"), each = 2L),
        dataset_id = rep(1:4, each = 2L),
        region = rep(
          c("North America", "North America", "Europe", "Europe"),
          each = 2L
        ),
        climatezone = rep(
          c("Polar", "Temperate", "Polar", "Temperate"),
          each = 2L
        ),
        predictor = rep(c("human", "climate"), 4L),
        individual = c(-0.2, 1.2, 0.1, 0.9, 0.7, 0.3, 0.8, 0.2),
        total_adjusted_r_squared = 1,
        has_negative_individual = rep(
          c(TRUE, FALSE, FALSE, FALSE),
          each = 2L
        ),
        is_importance_eligible = TRUE
      )
    data_meta <-
      tibble::tibble(
        dataset_id = 1:4,
        long = c(-100, -90, 10, 20),
        lat = c(50, 55, 50, 55),
        region = c(
          "North America", "North America", "Europe", "Europe"
        ),
        climatezone = c("Polar", "Temperate", "Polar", "Temperate")
      )

    data_geo_koppen <-
      tibble::tibble(
        x = c(-100, -90, 10, 20),
        y = c(50, 55, 50, 55),
        climatezone = factor(
          c("Polar", "Temperate", "Polar", "Temperate")
        )
      )

    result <-
      suppressWarnings(
        plot_hvarpart_spatial_balance(
          data_importance = data_importance,
          data_meta = data_meta,
          data_geo_koppen = data_geo_koppen,
          show_intervals = TRUE
        )
      )
    testthat::expect_s3_class(result$plot, "ggplot")
    testthat::expect_s3_class(result$statistical_plot, "ggplot")
    testthat::expect_equal(
      result[["statistical_plot"]][["labels"]][["y"]],
      paste0(
        "Relative importance\n",
        "(Zero-truncated human\u2212climate balance)"
      )
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
    testthat::expect_equal(
      result$record_values[["importance_balance"]],
      c(-1, -0.8, 0.4, 0.6)
    )
    testthat::expect_true(
      all(
        result$record_values[["importance_balance"]] >= -1 &
          result$record_values[["importance_balance"]] <= 1
      )
    )
    testthat::expect_true(
      "Continent" %in%
        as.character(result[["density_values"]][["panel_label"]])
    )
    testthat::expect_equal(
      result$region_values[["importance_balance"]],
      c(-10 / 11, 0.5)
    )
    testthat::expect_equal(
      result$statistical_plot$coordinates$limits$y,
      c(-1.12, 1.12)
    )
    dashed_zero_layers <-
      result[["statistical_plot"]][["layers"]] |>
      purrr::keep(
        ~ inherits(.x[["geom"]], "GeomHline") &&
          identical(.x[["aes_params"]][["linetype"]], 2)
      )
    testthat::expect_length(dashed_zero_layers, 1)
    summary_point_layers <-
      result[["statistical_plot"]][["layers"]] |>
      purrr::keep(~ inherits(.x[["geom"]], "GeomPoint"))
    testthat::expect_length(summary_point_layers, 1)
    interval_layers <-
      result[["statistical_plot"]][["layers"]] |>
      purrr::keep(~ inherits(.x[["geom"]], "GeomSegment"))
    testthat::expect_length(
      interval_layers,
      3
    )
    testthat::expect_s3_class(
      result[["statistical_plot"]][["theme"]][["panel.border"]],
      "element_blank"
    )
    testthat::expect_s3_class(
      result[["statistical_plot"]][["theme"]][["axis.line.y.right"]],
      "element_line"
    )
    plot_theme <- result[["statistical_plot"]][["theme"]]
    legend_background <- plot_theme[["legend.box.background"]]
    testthat::expect_s3_class(legend_background, "element_rect")
    rect_layers <-
      result[["statistical_plot"]][["layers"]] |>
      purrr::keep(
        ~ "background_balance" %in% names(.x[["data"]])
      )
    testthat::expect_length(rect_layers, 1)
    testthat::expect_false(
      "Continent" %in%
        as.character(rect_layers[[1]]$data[["panel_label"]])
    )
  }
)

testthat::test_that(
  "plot_hvarpart_spatial_balance() validates predictor pairs",
  {
    data_importance <-
      tibble::tibble(
        analysis = "spatial_spd",
        model_id = "one",
        dataset_id = 1,
        region = "Europe",
        climatezone = "Temperate",
        predictor = "human",
        individual = 0.5,
        total_adjusted_r_squared = 0.5,
        has_negative_individual = FALSE,
        is_importance_eligible = TRUE
      )
    data_meta <-
      tibble::tibble(
        dataset_id = 1,
        long = 10,
        lat = 50,
        region = "Europe",
        climatezone = "Temperate"
      )

    testthat::expect_error(
      plot_hvarpart_spatial_balance(
        data_importance = data_importance,
        data_meta = data_meta,
        data_geo_koppen = tibble::tibble()
      ),
      "finite human and climate allocations"
    )
  }
)
