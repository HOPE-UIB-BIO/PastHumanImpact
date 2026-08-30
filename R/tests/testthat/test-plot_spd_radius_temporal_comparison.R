testthat::test_that("plot_spd_radius_temporal_comparison() builds panels", {
  data_all <-
    tidyr::expand_grid(
      radius_km = c(250L, 500L),
      age = c(2000, 2500)
    ) |>
    dplyr::mutate(
      region = "Europe",
      status = "spatial_model_estimated",
      controlled_human = c(-0.2, -0.1, 0.1, 0.2),
      controlled_ranking = dplyr::if_else(
        .data[["controlled_human"]] > 0,
        "human",
        "climate"
      )
    )
  data_paired <-
    tibble::tibble(
      region = "Europe",
      age = c(2000, 2500, 3000),
      controlled_human_delta_500_minus_250 = c(0.3, 0.3, NA_real_),
      controlled_ranking_500_km = "human",
      material_change = c(TRUE, FALSE, TRUE),
      matched_estimable = c(TRUE, TRUE, FALSE)
    )

  result <-
    plot_spd_radius_temporal_comparison(data_all, data_paired)

  testthat::expect_named(result, c("profiles", "changes"))
  testthat::expect_s3_class(result[["profiles"]], "ggplot")
  testthat::expect_s3_class(result[["changes"]], "ggplot")
  scale_values <- result[["profiles"]]$scales$get_scales("fill")
  scale_delta <- result[["changes"]]$scales$get_scales("fill")
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
  testthat::expect_equal(
    unname(result[["profiles"]]$scales$get_scales("shape")$palette(2)),
    c(21, 22)
  )
  testthat::expect_gte(
    result[["profiles"]][["coordinates"]][["limits"]][["y"]][[2]],
    0.2
  )
  testthat::expect_equal(
    result[["changes"]][["coordinates"]][["limits"]][["y"]],
    c(-0.1, 0.1)
  )
  testthat::expect_null(
    result[["changes"]]$scales$get_scales("shape")
  )
  testthat::expect_null(
    result[["changes"]][["labels"]][["shape"]]
  )
  testthat::expect_equal(
    result[["profiles"]][["labels"]][["y"]],
    paste(
      "Relative importance",
      "(Untruncated hierarchical contribution)",
      sep = "\n"
    )
  )
  testthat::expect_equal(
    result[["changes"]][["labels"]][["y"]],
    paste(
      "Change in relative importance",
      "(Untruncated hierarchical human contribution; 500 km - 250 km)",
      sep = "\n"
    )
  )
  testthat::expect_equal(
    result[["profiles"]][["labels"]][["fill"]],
    "Human contribution"
  )
  testthat::expect_identical(
    result[["profiles"]][["labels"]][["x"]],
    "Age (cal ka BP)"
  )
  testthat::expect_identical(
    result[["changes"]][["labels"]][["x"]],
    "Age (cal ka BP)"
  )
  testthat::expect_equal(
    result[["profiles"]][["labels"]][["tag"]],
    "A"
  )
  testthat::expect_equal(
    result[["changes"]][["labels"]][["tag"]],
    "B"
  )
  testthat::expect_true(
    any(purrr::map_lgl(
      result[["profiles"]][["layers"]],
      ~ inherits(.x[["geom"]], "GeomSegment")
    ))
  )
  testthat::expect_true(
    any(purrr::map_lgl(
      result[["changes"]][["layers"]],
      ~ inherits(.x[["geom"]], "GeomSegment")
    ))
  )
  profile_line <-
    result[["profiles"]][["layers"]] |>
    purrr::keep(~ inherits(.x[["geom"]], "GeomLine")) |>
    purrr::pluck(1)
  profile_point <-
    result[["profiles"]][["layers"]] |>
    purrr::keep(~ inherits(.x[["geom"]], "GeomPoint")) |>
    purrr::pluck(1)
  change_line <-
    result[["changes"]][["layers"]] |>
    purrr::keep(~ inherits(.x[["geom"]], "GeomLine")) |>
    purrr::pluck(1)
  change_point <-
    result[["changes"]][["layers"]] |>
    purrr::keep(~ inherits(.x[["geom"]], "GeomPoint")) |>
    purrr::pluck(1)
  change_arrow_head <-
    result[["changes"]][["layers"]] |>
    purrr::keep(~ inherits(.x[["geom"]], "GeomSegment")) |>
    purrr::pluck(2) |>
    purrr::pluck("data")

  testthat::expect_gte(profile_line[["aes_params"]][["linewidth"]], 0.7)
  testthat::expect_gte(profile_point[["aes_params"]][["size"]], 4)
  testthat::expect_gte(change_line[["aes_params"]][["linewidth"]], 0.7)
  testthat::expect_gte(change_point[["aes_params"]][["size"]], 4)
  testthat::expect_gt(
    change_arrow_head[["xend"]],
    min(data_all[["age"]]) / 1000
  )
  testthat::expect_lt(
    change_arrow_head[["y"]],
    -0.15
  )
})
