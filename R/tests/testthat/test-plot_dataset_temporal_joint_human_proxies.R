testthat::test_that(
  "plot_dataset_temporal_trends treats KK10 and HYDE as human predictors",
  {
    variable_levels <- c("KK10", "HYDE", "Taxonomic richness")
    data_observed <-
      tibble::tibble(
        dataset_id = "d1",
        variable = c("kk10", "kk10", "hyde", "hyde", "n0"),
        variable_label = factor(
          c("KK10", "KK10", "HYDE", "HYDE", "Taxonomic richness"),
          levels = variable_levels
        ),
        region = "Europe",
        climatezone = "Temperate",
        age = c(1500, 2000, 1500, 2000, 2000),
        value = c(1, 2, 3, 4, 10)
      )
    data_raw <-
      tibble::tibble(
        dataset_id = "d1",
        variable = "n0",
        variable_label = factor("Taxonomic richness", levels = variable_levels),
        climatezone = "Temperate",
        age = 2000,
        value = 10
      )
    data_predictions <-
      tibble::tibble(
        dataset_id = "d1",
        variable = "n0",
        variable_label = factor("Taxonomic richness", levels = variable_levels),
        climatezone = "Temperate",
        age = 2000,
        estimate = 10,
        conf_low = 9,
        conf_high = 11
      )
    data_metadata <-
      tibble::tibble(
        dataset_id = "d1",
        long = 8.5,
        lat = 46.4,
        altitude = 1000,
        country = "Switzerland",
        depositionalenvironment = "Lake"
      )

    result <-
      plot_dataset_temporal_trends(
        data_raw = data_raw,
        data_observed = data_observed,
        data_predictions = data_predictions,
        data_metadata = data_metadata,
        climate_palette = c("Temperate" = "#371E71")
      )
    data_observed_layer <- result[["layers"]][[2]][["data"]]
    data_human <-
      dplyr::filter(
        data_observed_layer,
        .data[["variable"]] %in% c("kk10", "hyde")
      )

    testthat::expect_setequal(data_human[["variable"]], c("kk10", "hyde"))
    testthat::expect_true(all(data_human[["colour_group"]] == "Human"))
    testthat::expect_true(all(data_human[["age"]] >= 2000))
  }
)
