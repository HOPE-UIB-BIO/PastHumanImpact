testthat::test_that("spatial recreation validates its input contract", {
  testthat::expect_error(
    plot_hvarpart_spatial_composition(
      data_importance = tibble::tibble(),
      data_meta = tibble::tibble(),
      data_geo_koppen = tibble::tibble()
    ),
    "required contract"
  )
})

testthat::test_that("spatial recreation retains human profile values", {
  original_get_map_region <- build_region_map

  assign(
    "build_region_map",
    rlang::as_function(
      ~ ggplot2::ggplot() + ggplot2::theme_void()
    ),
    envir = globalenv()
  )

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
      individual = c(-0.2, 1.2, 0.1, 0.9, 0.3, 0.7, 0.4, 0.6),
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
        "North America",
        "North America",
        "Europe",
        "Europe"
      ),
      climatezone = c("Polar", "Temperate", "Polar", "Temperate")
    )

  result <-
    tryCatch(
      plot_hvarpart_spatial_composition(
        data_importance = data_importance,
        data_meta = data_meta,
        data_geo_koppen = tibble::tibble()
      ),
      finally = assign(
        "build_region_map",
        original_get_map_region,
        envir = globalenv()
      )
    )

  testthat::expect_s3_class(result[["main_plot"]], "ggplot")

  testthat::expect_s3_class(
    result[["signed_full_range_plot"]],
    "ggplot"
  )

  testthat::expect_identical(
    unique(result[["record_values"]][["predictor"]]),
    "human"
  )

  testthat::expect_equal(
    result[["record_values"]][["signed_allocation"]],
    c(-0.2, 0.1, 0.3, 0.4)
  )

  testthat::expect_equal(
    result[["record_values"]][["zero_truncated_allocation"]],
    c(0, 0.1, 0.3, 0.4)
  )

  testthat::expect_setequal(
    result[["summary_values"]][["profile"]],
    c("zero_truncated", "signed")
  )
})
