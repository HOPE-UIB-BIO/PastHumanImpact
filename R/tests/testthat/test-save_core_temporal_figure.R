testthat::test_that("save_core_temporal_figure writes and reuses a PNG", {
  path_output_dir <-
    tempfile(pattern = "core-temporal-figure-")
  dir.create(path_output_dir)

  data_observed <-
    tibble::tibble(
      dataset_id = "d1",
      variable = "n0",
      variable_label = factor("Taxonomic richness"),
      region = "Europe",
      climatezone = "Temperate",
      age = c(1000, 2000),
      value = c(10, 12)
    )
  data_predictions <-
    tibble::tibble(
      dataset_id = "d1",
      variable = "n0",
      variable_label = factor("Taxonomic richness"),
      climatezone = "Temperate",
      age = c(1000, 2000),
      estimate = c(10.5, 11.5),
      conf_low = c(9, 10),
      conf_high = c(12, 13)
    )
  data_raw <-
    data_observed
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

  path_first <-
    save_core_temporal_figure(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata,
      output_dir = path_output_dir,
      width = 120,
      height = 80,
      dpi = 72,
      verbose = FALSE
    )
  modified_first <-
    file.info(path_first)[["mtime"]]
  path_second <-
    save_core_temporal_figure(
      data_raw = data_raw,
      data_observed = data_observed,
      data_predictions = data_predictions,
      data_metadata = data_metadata,
      output_dir = path_output_dir,
      verbose = FALSE
    )

  testthat::expect_true(file.exists(path_first))
  testthat::expect_identical(path_second, path_first)
  testthat::expect_identical(
    file.info(path_second)[["mtime"]],
    modified_first
  )
})

testthat::test_that("save_core_temporal_figure validates dataset IDs", {
  testthat::expect_error(
    save_core_temporal_figure(
      data_raw = tibble::tibble(dataset_id = "invalid/id"),
      data_observed = tibble::tibble(dataset_id = "invalid/id"),
      data_predictions = tibble::tibble(dataset_id = "invalid/id"),
      data_metadata = tibble::tibble(),
      output_dir = tempdir(),
      verbose = FALSE
    ),
    regexp = "filename-safe"
  )
})
