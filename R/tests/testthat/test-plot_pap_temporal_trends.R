testthat::test_that("plot_pap_temporal_trends builds both layouts", {
  data_observed <-
    tibble::tibble(
      variable = "n0",
      pap_label = "Taxonomic richness",
      region = factor("Europe"),
      climatezone = factor("Temperate"),
      climatezone_label = factor("TMP"),
      dataset_id = factor("d1"),
      age = c(500, 1000),
      value = c(4, 5)
    )
  data_predictions <-
    tibble::tibble(
      variable = "n0",
      pap_label = "Taxonomic richness",
      region = factor("Europe"),
      climatezone = factor("Temperate"),
      climatezone_label = factor("TMP"),
      age = c(500, 1000),
      estimate = c(4.2, 4.8),
      conf_low = c(3.8, 4.3),
      conf_high = c(4.6, 5.3)
    )
  climate_palette <-
    c("Temperate" = "#371E71")

  plot_primary <-
    plot_pap_temporal_trends(
      data_observed = data_observed,
      data_predictions = data_predictions,
      layout = "primary",
      climate_palette = climate_palette
    )
  plot_strata <-
    plot_pap_temporal_trends(
      data_observed = data_observed,
      data_predictions = data_predictions,
      layout = "strata",
      climate_palette = climate_palette
    )

  testthat::expect_s3_class(plot_primary, "ggplot")
  testthat::expect_s3_class(plot_strata, "ggplot")
  testthat::expect_s3_class(plot_primary[["facet"]], "FacetGrid")
  testthat::expect_s3_class(plot_strata[["facet"]], "FacetGrid")
  testthat::expect_length(plot_primary[["layers"]], 2L)
  testthat::expect_length(plot_strata[["layers"]], 3L)
  testthat::expect_identical(
    plot_strata[["layers"]][[1]][["data"]],
    data_observed
  )
})

testthat::test_that("plot_pap_temporal_trends validates columns", {
  testthat::expect_error(
    plot_pap_temporal_trends(
      data_observed = tibble::tibble(variable = "n0"),
      data_predictions = tibble::tibble(variable = "n0"),
      climate_palette = c("Temperate" = "#371E71")
    ),
    regexp = "missing required columns"
  )
})
