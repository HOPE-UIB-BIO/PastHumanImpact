testthat::test_that("temporal balance plot uses lollipops", {
  data_balance <-
    tibble::tibble(
      analysis = c("temporal_spd", "temporal_events"),
      region = "Europe",
      age = 2000,
      human = c(0.25, 0.7),
      climate = c(0.75, 0.3),
      importance_balance = c(-0.5, 0.4)
    )

  result <- plot_hvarpart_temporal_balance(data_balance)

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomSegment"),
    logical(1)
  )))
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomLine"),
    logical(1)
  )))
  testthat::expect_false(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomCol"),
    logical(1)
  )))
  testthat::expect_equal(
    result$labels$y,
    paste(
      "Zero-truncated human-climate importance balance",
      "(human share \u2212 climate share)",
      sep = "\n"
    )
  )
  testthat::expect_equal(
    result$coordinates$limits$y,
    c(-1.2, 1.2)
  )
  testthat::expect_equal(
    result$scales$get_scales("x")$breaks,
    seq(0, 8.5, 0.5)
  )
  testthat::expect_equal(
    result$scales$get_scales("x")$limits,
    c(-8.5, 0)
  )
  testthat::expect_false("age_display" %in% names(result$data))
  testthat::expect_identical(result$coordinates$clip, "on")
  testthat::expect_s3_class(result$facet, "FacetGrid")
  testthat::expect_identical(result$theme$strip.placement, "outside")
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) {
      inherits(layer$geom, "GeomHline") &&
        identical(layer$aes_params$linetype, 2)
    },
    logical(1)
  )))
  testthat::expect_equal(
    unname(result$scales$get_scales("linetype")$palette(2)),
    c("solid", "dotted")
  )
})

testthat::test_that("temporal balance plot validates its contract", {
  testthat::expect_error(
    plot_hvarpart_temporal_balance(tibble::tibble()),
    "columns are missing or invalid"
  )
})

testthat::test_that("temporal gradient-background style is canonical", {
  data_balance <-
    tibble::tibble(
      analysis = c("temporal_spd", "temporal_events"),
      region = "Europe",
      age = 2000,
      importance_balance = c(-0.5, 0.4)
    )

  standard <- plot_hvarpart_temporal_balance(data_balance)
  white_fallback <- plot_hvarpart_temporal_balance(
    data_balance,
    background = "white"
  )

  count_rects <- function(plot) {
    sum(vapply(
      plot$layers,
      function(layer) inherits(layer$geom, "GeomRect"),
      logical(1)
    ))
  }
  point_layer <- standard$layers[[which(vapply(
    standard$layers,
    function(layer) inherits(layer$geom, "GeomPoint"),
    logical(1)
  ))]]

  testthat::expect_equal(count_rects(standard), 1)
  testthat::expect_equal(count_rects(white_fallback), 0)
  rect_layer <- standard$layers[[which(vapply(
    standard$layers,
    function(layer) inherits(layer$geom, "GeomRect"),
    logical(1)
  ))]]
  testthat::expect_equal(rect_layer$aes_params$alpha, 0.25)
  testthat::expect_equal(
    point_layer$aes_params$stroke,
    line_size * 7
  )
})

testthat::test_that("temporal balance plot enforces its age domain", {
  data_balance <-
    tibble::tibble(
      analysis = c(
        "temporal_spd",
        "temporal_spd",
        "temporal_spd",
        "temporal_events",
        "temporal_events"
      ),
      region = "Europe",
      age = c(1000, 2000, 9000, 0, 8500),
      importance_balance = 0
    )

  result <- plot_hvarpart_temporal_balance(data_balance)

  testthat::expect_equal(
    result$data |>
      dplyr::select(dplyr::all_of(c("analysis", "age"))),
    tibble::tibble(
      analysis = c("temporal_spd", "temporal_events", "temporal_events"),
      age = c(2000, 0, 8500)
    )
  )
})
