testthat::test_that("plot_core_temporal_trends plots raw and fitted data", {
  variable_levels <-
    c("SPD", "Taxonomic richness")
  data_observed <-
    tibble::tibble(
      dataset_id = "d1",
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
      dataset_id = "d1",
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
    dplyr::mutate(climatezone = factor(climatezone))
  data_metadata <-
    tibble::tibble(
      dataset_id = "d1",
      handle = "Core one",
      long = 8.5,
      lat = 46.4,
      altitude = 1000,
      country = "Switzerland",
      depositionalenvironment = "Lake"
    )

  result <-
    plot_core_temporal_trends(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata,
      climate_palette = c("Temperate" = "#371E71")
    )

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_s3_class(result[["facet"]], "FacetGrid")
  testthat::expect_length(result[["layers"]], 5L)
  testthat::expect_identical(
    result[["layers"]][[1]][["data"]][["variable"]],
    data_raw[["variable"]]
  )
  testthat::expect_setequal(
    as.character(
      result[["layers"]][[1]][["data"]][["colour_group"]]
    ),
    c("Human", "PAP")
  )
})

testthat::test_that("plot_core_temporal_trends requires one matching core", {
  data_observed <-
    tibble::tibble(
      dataset_id = c("d1", "d2"),
      variable = "n0",
      variable_label = "Taxonomic richness",
      region = "Europe",
      climatezone = "Temperate",
      age = 1000,
      value = 10
    )
  data_predictions <-
    tibble::tibble(
      dataset_id = "d1",
      variable = "n0",
      variable_label = "Taxonomic richness",
      climatezone = "Temperate",
      age = 1000,
      estimate = 10,
      conf_low = 9,
      conf_high = 11
    )
  data_raw <-
    data_observed %>%
    dplyr::filter(dataset_id == "d1")
  data_metadata <-
    tibble::tibble(
      long = 8.5,
      lat = 46.4,
      altitude = 1000,
      country = "Switzerland",
      depositionalenvironment = "Lake"
    )

  testthat::expect_error(
    plot_core_temporal_trends(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata,
      climate_palette = c("Temperate" = "#371E71")
    ),
    regexp = "same single dataset"
  )
})

testthat::test_that("plot_core_temporal_trends validates columns", {
  testthat::expect_error(
    plot_core_temporal_trends(
      data_raw = tibble::tibble(dataset_id = "d1"),
      data_observed = tibble::tibble(dataset_id = "d1"),
      data_predictions = tibble::tibble(dataset_id = "d1"),
      data_metadata = tibble::tibble()
    ),
    regexp = "missing required columns"
  )
})
