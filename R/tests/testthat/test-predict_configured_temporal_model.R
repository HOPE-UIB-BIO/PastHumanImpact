testthat::test_that("pending configured predictions are skipped", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-prediction-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")
  path_prediction_dir <-
    file.path(path_test_root, "predictions")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)
  dir.create(path_prediction_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")
  path_prediction_dir <-
    normalizePath(path_prediction_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = TRUE,
      need_to_be_evaluated = FALSE
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  result <-
    predict_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      verbose = FALSE
    )

  testthat::expect_null(result)
})

testthat::test_that("configured predictions update lifecycle state", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-prediction-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")
  path_prediction_dir <-
    file.path(path_test_root, "predictions")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)
  dir.create(path_prediction_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")
  path_prediction_dir <-
    normalizePath(path_prediction_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = FALSE,
      need_to_be_evaluated = FALSE,
      prediction_written = FALSE,
      last_prediction_date = NA_character_,
      model_file_name = "model_a.qs"
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  rlang::local_bindings(
    load_brms_model_file = function(...) {
      structure(list(), class = "brmsfit")
    },
    get_model_newdata = function(...) {
      tibble::tibble(dataset_id = "d1", age = 0)
    },
    predict_brms_model = function(...) {
      tibble::tibble(
        age = 0,
        estimate = 1,
        estimate_error = 0.1,
        conf_low = 0.8,
        conf_high = 1.2
      )
    },
    .env = globalenv()
  )

  result <-
    predict_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      verbose = FALSE
    )
  config_updated <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = path_config_dir,
      verbose = FALSE
    )
  prediction_saved <-
    RUtilpol::get_latest_file(
      file_name = "model_a",
      dir = path_prediction_dir,
      verbose = FALSE
    )

  testthat::expect_equal(result[["value"]], 1)
  testthat::expect_true(config_updated[["prediction_written"]])
  testthat::expect_false(is.na(
    config_updated[["last_prediction_date"]]
  ))
  testthat::expect_equal(prediction_saved[["estimate"]], 1)
})

testthat::test_that("dataset predictions do not update general lifecycle", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-core-prediction-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")
  path_prediction_dir <-
    file.path(path_test_root, "predictions")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)
  dir.create(path_prediction_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")
  path_prediction_dir <-
    normalizePath(path_prediction_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = FALSE,
      need_to_be_evaluated = FALSE,
      prediction_written = FALSE,
      last_prediction_date = NA_character_,
      model_file_name = "model_a.qs"
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  rlang::local_bindings(
    load_brms_model_file = function(...) {
      structure(list(), class = "brmsfit")
    },
    get_model_newdata = function(...) {
      tibble::tibble(dataset_id = "d1", age = 0)
    },
    predict_brms_model = function(...) {
      tibble::tibble(
        dataset_id = "d1",
        age = 0,
        estimate = 1,
        prediction_estimand = "dataset_specific",
        source_model_file = "model_a.qs",
        prediction_range = "group_observed"
      )
    },
    .env = globalenv()
  )

  result <-
    predict_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      prediction_range = "group_observed",
      prediction_estimand = "dataset_specific",
      verbose = FALSE
    )
  config_after <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = path_config_dir,
      verbose = FALSE
    )

  testthat::expect_equal(result[["estimate"]], 1)
  testthat::expect_false(config_after[["prediction_written"]])
  testthat::expect_true(is.na(config_after[["last_prediction_date"]]))
})

testthat::test_that("cached dataset IDs retain the prediction contract", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-cached-prediction-")
  path_config_dir <- file.path(path_test_root, "config")
  path_model_dir <- file.path(path_test_root, "models")
  path_prediction_dir <- file.path(path_test_root, "predictions")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)
  dir.create(path_prediction_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")
  path_prediction_dir <-
    normalizePath(path_prediction_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = FALSE,
      need_to_be_evaluated = FALSE,
      prediction_written = FALSE,
      model_file_name = "model_a.qs"
    )
  data_prediction <-
    tibble::tibble(
      dataset_id = 40579,
      estimate = 1,
      source_model_file = "model_a.qs",
      prediction_estimand = "dataset_specific",
      prediction_range = "group_observed"
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )
  RUtilpol::save_latest_file(
    object_to_save = data_prediction,
    file_name = "model_a",
    dir = path_prediction_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  result <-
    predict_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      prediction_range = "group_observed",
      prediction_estimand = "dataset_specific",
      verbose = FALSE
    )

  testthat::expect_identical(result[["dataset_id"]], "40579")
})

testthat::test_that("stale configured predictions are regenerated", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-prediction-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")
  path_prediction_dir <-
    file.path(path_test_root, "predictions")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)
  dir.create(path_prediction_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")
  path_prediction_dir <-
    normalizePath(path_prediction_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = FALSE,
      need_to_be_evaluated = FALSE,
      prediction_written = TRUE,
      last_prediction_date = "2026-07-01",
      model_file_name = "model_a_current.qs"
    )
  data_stale_prediction <-
    tibble::tibble(
      estimate = 0,
      source_model_file = "model_a_old.qs",
      prediction_estimand = "first_dataset"
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )
  RUtilpol::save_latest_file(
    object_to_save = data_stale_prediction,
    file_name = "model_a",
    dir = path_prediction_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  rlang::local_bindings(
    load_brms_model_file = function(...) {
      structure(list(), class = "brmsfit")
    },
    get_model_newdata = function(...) {
      tibble::tibble(dataset_id = "d1", age = 0)
    },
    predict_brms_model = function(...) {
      tibble::tibble(
        estimate = 2,
        source_model_file = "model_a_current.qs",
        prediction_estimand = "equal_weighted_dataset_mean"
      )
    },
    .env = globalenv()
  )

  result <-
    predict_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      prediction_dir = path_prediction_dir,
      verbose = FALSE
    )

  testthat::expect_equal(result[["estimate"]], 2)
  testthat::expect_identical(
    result[["source_model_file"]],
    "model_a_current.qs"
  )
})
