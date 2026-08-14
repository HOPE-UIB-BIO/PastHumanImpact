testthat::test_that("ineligible configured models are skipped", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-model-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = FALSE
    )

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  result <-
    run_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      path_history = file.path(path_test_root, "history.csv"),
      request_id = "request_a",
      verbose = FALSE
    )

  testthat::expect_null(result)
  testthat::expect_false(file.exists(file.path(path_test_root, "history.csv")))
})

testthat::test_that("configured models without pending work are skipped", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-model-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")

  data_config <-
    tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE,
      need_to_run = FALSE,
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
    run_configured_temporal_model(
      model_id = "model_a",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      path_history = file.path(path_test_root, "history.csv"),
      request_id = "request_a",
      verbose = FALSE
    )

  testthat::expect_null(result)
})

testthat::test_that("configured model IDs must match exactly one row", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-model-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")

  RUtilpol::save_latest_file(
    object_to_save = tibble::tibble(
      model_id = "model_a",
      is_model_eligible = TRUE
    ),
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  testthat::expect_error(
    run_configured_temporal_model(
      model_id = "unknown_model",
      data_source = tibble::tibble(),
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      path_history = file.path(path_test_root, "history.csv"),
      request_id = "request_a",
      verbose = FALSE
    ),
    regexp = "exactly one"
  )
})

testthat::test_that("failed configured fits advance and record the seed", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-model-")
  path_config_dir <-
    file.path(path_test_root, "config")
  path_model_dir <-
    file.path(path_test_root, "models")
  path_history <-
    file.path(path_test_root, "history.csv")

  dir.create(path_config_dir, recursive = TRUE)
  dir.create(path_model_dir, recursive = TRUE)

  path_config_dir <-
    normalizePath(path_config_dir, winslash = "/")
  path_model_dir <-
    normalizePath(path_model_dir, winslash = "/")

  data_model <-
    tibble::tibble(
      dataset_id = c("d1", "d1", "d2", "d2"),
      region = "Europe",
      climatezone = "Temperate",
      stratum = "Europe__Temperate",
      variable = "n0",
      age_ka = c(0, 1, 0, 1),
      value = c(1, 2, 2, 3)
    )

  data_config <-
    build_model_config_table(
      data_model = data_model,
      analysis = "pap_temporal",
      family_key = "invalid_family",
      total_iterations = 100L,
      min_iterations_per_chain = 100L,
      max_chains = 1L
    )

  initial_seed <-
    data_config[["sampling_seed"]][1]

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )

  result <-
    run_configured_temporal_model(
      model_id = data_config[["model_id"]][1],
      data_source = data_model,
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      path_history = path_history,
      request_id = "request_a",
      verbose = FALSE
    )

  data_history <-
    readr::read_csv(
      path_history,
      show_col_types = FALSE
    )

  testthat::expect_true(result[["need_to_run"]])
  testthat::expect_false(result[["need_to_be_evaluated"]])
  testthat::expect_equal(result[["seed_attempt"]], 2L)
  testthat::expect_false(result[["sampling_seed"]] == initial_seed)
  testthat::expect_identical(result[["last_run_seed"]], initial_seed)
  testthat::expect_identical(
    data_history[["event"]],
    c("fit_started", "fit_failed")
  )
  testthat::expect_true(all(data_history[["run_seed"]] == initial_seed))
})
