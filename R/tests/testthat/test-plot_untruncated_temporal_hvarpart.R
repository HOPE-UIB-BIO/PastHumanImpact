testthat::test_that("temporal plot accepts negative and above-one values", {
  data_summary <- tibble::tibble(
    analysis = rep(c("temporal_spd", "temporal_events"), each = 2L),
    region = "Europe",
    age = 1000,
    predictor = rep(c("human", "climate"), 2L),
    pooled_allocation = c(-0.2, 1.2, 0.3, 0.7)
  )

  result <- plot_untruncated_temporal_hvarpart(data_summary)

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomCol"),
    logical(1)
  )))
  testthat::expect_false(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomLine"),
    logical(1)
  )))
  testthat::expect_identical(
    levels(result$data[["predictor_proxy"]]),
    c(
      "Human (SPD)", "Human (Events)",
      "Climate (SPD)", "Climate (Events)"
    )
  )
  testthat::expect_identical(
    levels(result$data[["predictor"]]),
    c("human", "climate")
  )
  testthat::expect_length(
    result$scales$get_scales("fill")$palette(4),
    4L
  )
  testthat::expect_identical(
    tail(levels(result$data[["age_label"]]), 1L),
    "0.5"
  )
  testthat::expect_false("0.0" %in% levels(result$data[["age_label"]]))
  testthat::expect_identical(
    result$scales$get_scales("y")$position,
    "right"
  )
  testthat::expect_equal(
    as.numeric(result$theme$plot.margin[[4]]),
    8
  )
  testthat::expect_identical(
    result[["labels"]][["x"]],
    "Age (cal ka BP)"
  )
  testthat::expect_identical(
    result[["labels"]][["y"]],
    "Relative importance\n(Untruncated hierarchical contribution)"
  )
  region_labels <-
    result$facet$params$labeller(
      data.frame(region = "Latin America")
    )
  testthat::expect_identical(
    region_labels[["region"]],
    "Central &\nSouth America"
  )
  testthat::expect_s3_class(result$facet, "FacetGrid")
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) {
      inherits(layer$geom, "GeomHline") &&
        identical(layer$aes_params$linetype, 2)
    },
    logical(1)
  )))
})

testthat::test_that("signed temporal plot excludes young SPD values", {
  data_summary <- tibble::tibble(
    analysis = c("temporal_spd", "temporal_spd", "temporal_events"),
    region = "Europe",
    age = c(1000, 2000, 1000),
    predictor = "human",
    pooled_allocation = 0.5
  )

  result <- plot_untruncated_temporal_hvarpart(data_summary)

  testthat::expect_false(any(
    result$data[["analysis"]] == "temporal_spd" &
      result$data[["age"]] < 2000
  ))
})
