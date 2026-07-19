testthat::test_that("save_pap_temporal_figure writes PNG and PDF", {
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
  path_output <-
    tempfile(pattern = "pap-figure-")
  dir.create(path_output)

  result <-
    save_pap_temporal_figure(
      data_observed = data_observed,
      data_predictions = data_predictions,
      pap_variables = "n0",
      layout = "strata",
      output_dir = path_output,
      output_stem = "pap_n0",
      width = 80,
      height = 60,
      dpi = 72
    )

  testthat::expect_s3_class(result, "ggplot")
  testthat::expect_true(
    file.exists(file.path(path_output, "pap_n0.png"))
  )
  testthat::expect_true(
    file.exists(file.path(path_output, "pap_n0.pdf"))
  )
})

testthat::test_that("save_pap_temporal_figure validates variables", {
  testthat::expect_error(
    save_pap_temporal_figure(
      data_observed = tibble::tibble(variable = "n0"),
      data_predictions = tibble::tibble(variable = "n0"),
      pap_variables = "roc",
      layout = "strata",
      output_dir = tempdir(),
      output_stem = "pap_roc",
      width = 80,
      height = 60
    ),
    regexp = "every requested variable"
  )
})
