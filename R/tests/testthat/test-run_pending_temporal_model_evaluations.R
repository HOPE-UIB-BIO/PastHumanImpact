testthat::test_that("evaluation runner skips when nothing is pending", {
  path_config <-
    tempfile()

  path_models <-
    tempfile()

  dir.create(path_config)

  dir.create(path_models)

  path_config <-
    normalizePath(path_config, winslash = "/", mustWork = TRUE)

  path_models <-
    normalizePath(path_models, winslash = "/", mustWork = TRUE)

  RUtilpol::save_latest_file(
    object_to_save = tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_be_evaluated = FALSE
    ),
    file_name = "general_model_config_table",
    dir = path_config,
    prefered_format = "csv",
    verbose = FALSE
  )

  result <-
    run_pending_temporal_model_evaluations(
      config_dir = path_config,
      model_dir = path_models,
      path_history = tempfile(),
      verbose = FALSE
    )

  testthat::expect_equal(nrow(result), 0L)
})
