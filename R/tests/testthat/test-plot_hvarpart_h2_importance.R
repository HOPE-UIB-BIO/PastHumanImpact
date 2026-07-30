testthat::test_that("H2 importance plot supports balance and signed profiles", {
  data_summary <- tibble::tibble(
    region = "Europe",
    climatezone = "Polar",
    predictor = c("human", "climate"),
    pooled_allocation = c(-0.2, 1.2)
  )

  signed <- plot_hvarpart_h2_importance(
    data_summary = data_summary,
    selected_region = "Europe",
    selected_climatezone = "Polar",
    profile = "signed"
  )
  zero <- plot_hvarpart_h2_importance(
    data_summary = dplyr::mutate(
      data_summary,
      pooled_allocation = c(0, 1)
    ),
    selected_region = "Europe",
    selected_climatezone = "Polar",
    profile = "zero_truncated"
  )

  testthat::expect_s3_class(signed, "ggplot")
  testthat::expect_s3_class(zero, "ggplot")
  testthat::expect_true(any(vapply(
    signed$layers,
    function(layer) inherits(layer$geom, "GeomHline"),
    logical(1)
  )))
  testthat::expect_true(any(vapply(
    zero$layers,
    function(layer) inherits(layer$geom, "GeomSegment"),
    logical(1)
  )))
  testthat::expect_true(any(vapply(
    zero$layers,
    function(layer) inherits(layer$geom, "GeomHline"),
    logical(1)
  )))
  testthat::expect_true(any(vapply(
    zero$layers,
    function(layer) inherits(layer$geom, "GeomRect"),
    logical(1)
  )))
  testthat::expect_false(any(vapply(
    zero$layers,
    function(layer) inherits(layer$geom, "GeomCol"),
    logical(1)
  )))
  testthat::expect_equal(
    zero$data[["importance_balance"]],
    -1
  )
  testthat::expect_s3_class(
    zero$theme$panel.border,
    "element_rect"
  )
  point_layer <- zero$layers[[which(vapply(
    zero$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  ))]]
  rect_layer <- zero$layers[[which(vapply(
    zero$layers,
    function(layer) inherits(layer$geom, "GeomRect"),
    logical(1)
  ))]]
  testthat::expect_equal(point_layer$aes_params$stroke, line_size * 7)
  testthat::expect_equal(rect_layer$aes_params$alpha, 0.25)
  testthat::expect_equal(
    zero$scales$get_scales("x")$limits,
    c(0.2, 1.8)
  )
  testthat::expect_equal(
    zero$scales$get_scales("y")$limits,
    c(-1.3, 1.3)
  )
  testthat::expect_true(any(vapply(
    zero$layers,
    function(layer) {
      inherits(layer$geom, "GeomHline") &&
        identical(layer$aes_params$linetype, 2)
    },
    logical(1)
  )))
  testthat::expect_true(any(vapply(
    signed$layers,
    function(layer) {
      inherits(layer$geom, "GeomHline") &&
        identical(layer$aes_params$linetype, 2)
    },
    logical(1)
  )))
})

testthat::test_that("H2 importance plot validates profile strata", {
  testthat::expect_error(
    plot_hvarpart_h2_importance(
      data_summary = tibble::tibble(),
      selected_region = "Europe",
      selected_climatezone = "Polar"
    ),
    "required contract"
  )
})
