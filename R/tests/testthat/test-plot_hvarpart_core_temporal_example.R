testthat::test_that("plot_hvarpart_core_temporal_example() combines plots", {
  variable_levels <-
    c("SPD", "Taxonomic richness")
  data_observed <-
    tibble::tibble(
      dataset_id = "core_a",
      variable = c("spd", "spd", "n0", "n0"),
      variable_label = factor(
        c("SPD", "SPD", "Taxonomic richness", "Taxonomic richness"),
        levels = variable_levels
      ),
      region = "Europe",
      climatezone = "Temperate",
      age = c(1000, 2000, 1000, 2000),
      value = c(0.2, 0.4, 10, 12)
    )
  data_predictions <-
    tibble::tibble(
      dataset_id = "core_a",
      variable = c("spd", "spd", "n0", "n0"),
      variable_label = factor(
        c("SPD", "SPD", "Taxonomic richness", "Taxonomic richness"),
        levels = variable_levels
      ),
      climatezone = "Temperate",
      age = c(1000, 2000, 1000, 2000),
      estimate = c(0.25, 0.35, 10.5, 11.5),
      conf_low = c(0.1, 0.2, 9, 10),
      conf_high = c(0.4, 0.5, 12, 13)
    )
  data_raw <-
    data_observed %>%
    dplyr::mutate(climatezone = factor(.data[["climatezone"]]))
  data_metadata <-
    tibble::tibble(
      dataset_id = "core_a",
      handle = "Core A",
      long = 8.5,
      lat = 46.4,
      altitude = 1000,
      country = "Switzerland",
      depositionalenvironment = "Lake"
    )
  data_importance <-
    tibble::tibble(
      dataset_id = "core_a",
      predictor = c("human", "climate"),
      individual_percent = c(60, 40),
      total_adjusted_r_squared = 0.35
    )

  res_plot <-
    plot_hvarpart_core_temporal_example(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata,
      data_importance = data_importance,
      climate_palette = c(Temperate = "#4C4C9D"),
      predictor_palette = c(
        human = "#D2A62C",
        climate = "#2A7F7F"
      )
    )

  testthat::expect_s3_class(res_plot, "ggplot")
})

testthat::test_that(
  "plot_hvarpart_core_temporal_example() requires one core",
  {
    testthat::expect_error(
      plot_hvarpart_core_temporal_example(
        data_raw = tibble::tibble(),
        data_observed = tibble::tibble(dataset_id = c("a", "b")),
        data_predictions = tibble::tibble(),
        data_metadata = tibble::tibble(),
        data_importance = tibble::tibble()
      ),
      "one dataset"
    )
  }
)
