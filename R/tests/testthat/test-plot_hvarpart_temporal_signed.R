testthat::test_that("temporal plot accepts negative and above-one values", {
  data_summary <- tibble::tibble(
    analysis = rep(c("temporal_spd", "temporal_events"), each = 2L),
    region = "Europe",
    age = 1000,
    predictor = rep(c("human", "climate"), 2L),
    pooled_allocation = c(-0.2, 1.2, 0.3, 0.7)
  )

  result <- plot_hvarpart_temporal_signed(data_summary)

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(any(vapply(
    result$layers,
    function(layer) inherits(layer$geom, "GeomHline"),
    logical(1)
  )))
})
