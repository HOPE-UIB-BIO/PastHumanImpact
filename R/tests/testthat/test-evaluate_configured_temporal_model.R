testthat::test_that("models without pending evaluation are skipped", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-evaluation-")
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
    evaluate_configured_temporal_model(
      model_id = "model_a",
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      path_history = file.path(path_test_root, "history.csv"),
      verbose = FALSE
    )

  testthat::expect_null(result)
  testthat::expect_false(file.exists(file.path(path_test_root, "history.csv")))
})

testthat::test_that("LOO-only failure is recorded without a rerun", {
  testthat::skip_if_not_installed("RUtilpol")

  path_test_root <-
    tempfile(pattern = "configured-evaluation-")
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
    create_model_config_table(
      data_model = data_model,
      analysis = "pap_temporal",
      family_key = "gamma_log",
      min_records = 1L,
      total_iterations = 100L,
      min_iterations_per_chain = 100L,
      max_chains = 1L
    ) %>%
    dplyr::mutate(
      need_to_run = FALSE,
      need_to_be_evaluated = TRUE,
      prediction_written = TRUE,
      last_prediction_date = "2026-07-01",
      last_run_id = "model_run_1",
      last_run_seed_attempt = seed_attempt,
      last_run_seed = sampling_seed
    )

  model_id <-
    data_config[["model_id"]][1]
  initial_seed <-
    data_config[["sampling_seed"]][1]

  RUtilpol::save_latest_file(
    object_to_save = data_config,
    file_name = "general_model_config_table",
    dir = path_config_dir,
    prefered_format = "csv",
    verbose = FALSE
  )
  RUtilpol::save_latest_file(
    object_to_save = structure(list(), class = "brmsfit"),
    file_name = model_id,
    dir = path_model_dir,
    prefered_format = "qs",
    verbose = FALSE
  )

  testthat::local_mocked_bindings(
    loo = function(...) {
      list(diagnostics = list(pareto_k = c(0.8, 0.9)))
    },
    rhat = function(...) c(1.00, 1.01),
    neff_ratio = function(...) c(0.5, 0.6),
    nuts_params = function(...) {
      tibble::tibble(
        Parameter = c("divergent__", "treedepth__"),
        Value = c(0, 5)
      )
    },
    .package = "brms"
  )

  result <-
    evaluate_configured_temporal_model(
      model_id = model_id,
      config_dir = path_config_dir,
      model_dir = path_model_dir,
      path_history = path_history,
      verbose = FALSE
    )

  data_history <-
    readr::read_csv(
      path_history,
      show_col_types = FALSE
    )

  testthat::expect_false(result[["need_to_run"]])
  testthat::expect_false(result[["need_to_be_evaluated"]])
  testthat::expect_false(result[["last_run_loo_test_pass"]])
  testthat::expect_equal(result[["sampling_seed"]], initial_seed)
  testthat::expect_identical(data_history[["event"]], "evaluation_failed")
  testthat::expect_equal(data_history[["run_seed"]], initial_seed)
})
