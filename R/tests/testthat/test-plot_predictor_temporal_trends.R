testthat::test_that("plot_predictor_temporal_trends() builds the plot", {
  data_observed <-
    tibble::tibble(
      region = factor(rep("Europe", 5)),
      climatezone = factor(
        rep("Temperate_Without_dry_season", 5)
      ),
      dataset_id = c("core_a", "core_a", "core_b", "core_b", "core_a"),
      age = c(2000, 4000, 2000, 4000, 2000),
      variable = c(rep("spd", 4), "temp_annual"),
      value = c(0.2, 0.4, 0.3, 0.5, 10)
    )
  data_predictions <-
    tibble::tibble(
      region = factor(rep("Europe", 3)),
      climatezone = factor(
        rep("Temperate_Without_dry_season", 3)
      ),
      age = c(2000, 4000, 2000),
      variable = c("spd", "spd", "temp_annual"),
      value = c(0.25, 0.45, 10),
      conf_low = c(0.15, 0.35, 9),
      conf_high = c(0.35, 0.55, 11)
    )
  climate_palette <-
    c("Temperate_Without_dry_season" = "#4C4C9D")

  res_plot <-
    plot_predictor_temporal_trends(
      data_observed = data_observed,
      data_predictions = data_predictions,
      variable = "spd",
      y_limits = c(0, 1),
      climate_palette = climate_palette
    )

  testthat::expect_s3_class(res_plot, "ggplot")
  testthat::expect_length(res_plot[["layers"]], 3L)
  testthat::expect_true(
    all(res_plot[["layers"]][[1]][["data"]][["variable"]] == "spd")
  )
})

testthat::test_that("plot_predictor_temporal_trends() validates inputs", {
  data_observed <-
    tibble::tibble(variable = "spd")
  data_predictions <-
    tibble::tibble(variable = "spd")

  testthat::expect_error(
    plot_predictor_temporal_trends(
      data_observed = data_observed,
      data_predictions = data_predictions,
      variable = "spd",
      y_limits = c(0, 1),
      climate_palette = c(zone = "black")
    ),
    "missing required columns"
  )
})
