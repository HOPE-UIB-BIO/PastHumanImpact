testthat::test_that("plot_spd_radius_spatial_comparison() builds panels", {
  data_all <-
    tidyr::expand_grid(
      dataset_id = c("a", "b"),
      radius_km = c(250L, 500L)
    ) |>
    dplyr::mutate(
      region = "Europe",
      signed_difference = c(-0.2, -0.1, 0.1, 0.2),
      zero_balance = c(-0.3, -0.1, 0.2, 0.4),
      signed_ranking = dplyr::if_else(
        .data[["signed_difference"]] > 0,
        "human",
        "climate"
      ),
      status = "estimated"
    )
  data_summary <-
    tibble::tibble(
      summary_level = c("region", "climatezone"),
      metric = "zero_balance",
      region = c("Europe", NA_character_),
      climatezone = c(NA_character_, "Temperate"),
      median_delta = c(0.1, -0.1),
      delta_q25 = c(0.05, -0.15),
      delta_q75 = c(0.15, -0.05)
    )

  result <-
    plot_spd_radius_spatial_comparison(data_all, data_summary)

  testthat::expect_s3_class(result, "patchwork")
  scale_values <- result[[1]]$scales$get_scales("fill")
  scale_delta <- result[[2]]$scales$get_scales("fill")
  scale_y <- result[[1]]$scales$get_scales("y")
  scale_x_delta <- result[[2]]$scales$get_scales("x")
  scale_shape_values <- result[[1]]$scales$get_scales("shape")
  testthat::expect_s3_class(
    scale_values,
    "ScaleContinuous"
  )
  testthat::expect_s3_class(
    scale_delta,
    "ScaleContinuous"
  )
  testthat::expect_equal(scale_values$limits, c(-1, 1))
  testthat::expect_equal(scale_delta$limits, c(-1, 1))
  testthat::expect_equal(scale_values$breaks, c(-1, 0, 1))
  testthat::expect_equal(scale_delta$breaks, c(-1, 0, 1))
  testthat::expect_equal(scale_y$limits, c(-1, 1))
  testthat::expect_equal(scale_x_delta$limits, c(-1, 1))
  testthat::expect_identical(scale_shape_values$guide, "none")
  testthat::expect_equal(
    unname(scale_shape_values$palette(2)),
    c(21, 22)
  )
  testthat::expect_null(result[[2]]$scales$get_scales("shape"))
  testthat::expect_equal(
    result[[1]][["labels"]][["y"]],
    "Zero-truncated human-climate importance balance"
  )
})
