testthat::test_that("signed spatial plot aligns panels and clamps colours", {
  original_get_map_region <- get_map_region
  assign(
    "get_map_region",
    function(...) ggplot2::ggplot() + ggplot2::theme_void(),
    envir = globalenv()
  )

  data_importance <-
    tibble::tibble(
      analysis = "spatial_spd",
      model_id = rep(paste0("model_", 1:4), each = 2L),
      dataset_id = rep(1:4, each = 2L),
      region = rep(c("North America", "Europe"), each = 4L),
      climatezone = rep(c("Polar", "Temperate"), each = 2L, times = 2L),
      predictor = rep(c("human", "climate"), 4L),
      individual = c(-0.2, 1.2, 0.2, 0.8, 0.4, 0.6, 1.2, -0.2),
      total_adjusted_r_squared = 1,
      has_negative_individual = rep(c(TRUE, FALSE, FALSE, TRUE), each = 2L),
      is_importance_eligible = TRUE
    )
  data_meta <-
    tibble::tibble(
      dataset_id = 1:4,
      long = c(-100, -90, 10, 20),
      lat = c(50, 55, 50, 55),
      region = c("North America", "North America", "Europe", "Europe"),
      climatezone = c("Polar", "Temperate", "Polar", "Temperate")
    )

  result <- tryCatch(
    plot_hvarpart_spatial_signed(
      data_importance = data_importance,
      data_meta = data_meta,
      data_geo_koppen = tibble::tibble()
    ),
    finally = assign(
      "get_map_region",
      original_get_map_region,
      envir = globalenv()
    )
  )

  testthat::expect_s3_class(result$plot, "ggplot")
  testthat::expect_s3_class(result$statistical_plot, "ggplot")
  testthat::expect_equal(
    range(dplyr::pull(result$record_values, signed_allocation)),
    c(-0.2, 1.2)
  )
  testthat::expect_true(all(
    c(
      "zero_truncated_individual",
      "zero_truncated_total",
      "zero_truncated_allocation"
    ) %in% names(result$record_values)
  ))
  continuous_fill_scales <-
    purrr::keep(
      result$statistical_plot$scales$scales,
      ~ inherits(.x, "ScaleContinuous")
    )
  testthat::expect_true(any(vapply(
    continuous_fill_scales,
    function(scale) identical(scale$limits, c(0, 1)),
    logical(1)
  )))
  background_layer <-
    purrr::detect(
      result$statistical_plot$layers,
      ~ inherits(.x$geom, "GeomRect")
    )
  testthat::expect_equal(background_layer$aes_params$alpha, 0.15)
  categorical_fill_scale <-
    result$statistical_plot$scales$get_scales("fill")
  testthat::expect_s3_class(categorical_fill_scale, "ScaleDiscrete")
  testthat::expect_equal(
    unname(categorical_fill_scale$palette(length(palette_ecozones))),
    unname(palette_ecozones)
  )
  testthat::expect_identical(
    result$statistical_plot$theme$legend.position,
    "bottom"
  )
  testthat::expect_identical(
    result$statistical_plot$theme$legend.box,
    "vertical"
  )
  testthat::expect_equal(
    result$statistical_plot$coordinates$limits$y,
    c(-0.5, 1.5)
  )
})

testthat::test_that("signed spatial plot validates its contract", {
  testthat::expect_error(
    plot_hvarpart_spatial_signed(
      data_importance = tibble::tibble(),
      data_meta = tibble::tibble(),
      data_geo_koppen = tibble::tibble()
    ),
    "required contract"
  )
})
