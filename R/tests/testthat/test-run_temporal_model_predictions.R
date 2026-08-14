testthat::test_that("prediction runner blocks pending fits", {
  path_config <-
    tempfile()

  path_models <-
    tempfile()

  path_predictions <-
    tempfile()

  dir.create(path_config)

  dir.create(path_models)

  dir.create(path_predictions)

  path_config <-
    normalizePath(path_config, winslash = "/", mustWork = TRUE)

  path_models <-
    normalizePath(path_models, winslash = "/", mustWork = TRUE)

  path_predictions <-
    normalizePath(
      path_predictions,
      winslash = "/",
      mustWork = TRUE
    )

  RUtilpol::save_latest_file(
    object_to_save = tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = TRUE,
      need_to_be_evaluated = FALSE
    ),
    file_name = "general_model_config_table",
    dir = path_config,
    prefered_format = "csv",
    verbose = FALSE
  )

  testthat::expect_error(
    run_temporal_model_predictions(
      data_source = tibble::tibble(),
      config_dir = path_config,
      model_dir = path_models,
      prediction_dir = path_predictions,
      verbose = FALSE
    ),
    regexp = "pass evaluation"
  )
})
