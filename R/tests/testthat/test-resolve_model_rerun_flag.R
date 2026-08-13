testthat::test_that("resolve_model_rerun_flag() updates selected row and saves table", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <-
    file.path(tempdir(), "phi-model-rerun")

  temp_storage_root_unix <-
    gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Temporal_models"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  had_data_storage_path <-
    exists("data_storage_path", inherits = TRUE)

  if (had_data_storage_path) {
    old_data_storage_path <-
      get("data_storage_path", envir = .GlobalEnv)
  }

  assign("data_storage_path", paste0(temp_storage_root_unix, "/"), envir = .GlobalEnv)

  on.exit({
    if (had_data_storage_path) {
      assign("data_storage_path", old_data_storage_path, envir = .GlobalEnv)
    } else if (exists("data_storage_path", envir = .GlobalEnv, inherits = FALSE)) {
      rm("data_storage_path", envir = .GlobalEnv)
    }
  }, add = TRUE)

  data_source <-
    data.frame(
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold"),
      variable = c("temp_annual", "spd"),
      need_to_be_evaluated = c(TRUE, TRUE),
      need_to_run = c(FALSE, FALSE),
      last_evaluation_date = c("2020-01-01", "2020-01-01"),
      stringsAsFactors = FALSE
    )

  resolve_model_rerun_flag(
    data_source = data_source,
    sel_region = "Europe",
    sel_climatezone = "Temperate",
    sel_variable = "temp_annual",
    advance_seed = FALSE
  )

  saved <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = paste0(temp_storage_root_unix, "/Temporal_models"),
      verbose = FALSE
    )

  testthat::expect_identical(saved[["need_to_be_evaluated"]], c(FALSE, TRUE))
  testthat::expect_identical(saved[["need_to_run"]], c(TRUE, FALSE))
  testthat::expect_equal(as.Date(saved[["last_evaluation_date"]][1]), Sys.Date())
})

testthat::test_that("resolve_model_rerun_flag() leaves unmatched rows unchanged", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <-
    file.path(tempdir(), "phi-model-rerun-unmatched")
  temp_storage_root_unix <-
    gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Temporal_models"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  had_data_storage_path <-
    exists("data_storage_path", inherits = TRUE)

  if (had_data_storage_path) {
    old_data_storage_path <-
      get("data_storage_path", envir = .GlobalEnv)
  }

  assign("data_storage_path", paste0(temp_storage_root_unix, "/"), envir = .GlobalEnv)

  on.exit({
    if (had_data_storage_path) {
      assign("data_storage_path", old_data_storage_path, envir = .GlobalEnv)
    } else if (exists("data_storage_path", envir = .GlobalEnv, inherits = FALSE)) {
      rm("data_storage_path", envir = .GlobalEnv)
    }
  }, add = TRUE)

  data_source <-
    data.frame(
      region = c("Europe", "Asia"),
      climatezone = c("Temperate", "Cold"),
      variable = c("temp_annual", "spd"),
      need_to_be_evaluated = c(TRUE, TRUE),
      need_to_run = c(FALSE, FALSE),
      last_evaluation_date = c("2020-01-01", "2020-01-01"),
      stringsAsFactors = FALSE
    )

  resolve_model_rerun_flag(
    data_source = data_source,
    sel_region = "Oceania",
    sel_climatezone = "Arid",
    sel_variable = "prec_annual",
    advance_seed = FALSE
  )

  saved <-
    RUtilpol::get_latest_file(
      file_name = "general_model_config_table",
      dir = paste0(temp_storage_root_unix, "/Temporal_models"),
      verbose = FALSE
    )

  testthat::expect_identical(saved[["need_to_be_evaluated"]], c(TRUE, TRUE))
  testthat::expect_identical(saved[["need_to_run"]], c(FALSE, FALSE))
  testthat::expect_identical(as.character(saved[["last_evaluation_date"]]), c("2020-01-01", "2020-01-01"))
})

testthat::test_that("resolve_model_rerun_flag() writes one saved table", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <-
    file.path(tempdir(), "phi-model-rerun-files")
  temp_storage_root_unix <-
    gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Temporal_models"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  had_data_storage_path <-
    exists("data_storage_path", inherits = TRUE)

  if (had_data_storage_path) {
    old_data_storage_path <-
      get("data_storage_path", envir = .GlobalEnv)
  }

  assign("data_storage_path", paste0(temp_storage_root_unix, "/"), envir = .GlobalEnv)

  on.exit({
    if (had_data_storage_path) {
      assign("data_storage_path", old_data_storage_path, envir = .GlobalEnv)
    } else if (exists("data_storage_path", envir = .GlobalEnv, inherits = FALSE)) {
      rm("data_storage_path", envir = .GlobalEnv)
    }
  }, add = TRUE)

  data_source <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      variable = "temp_annual",
      need_to_be_evaluated = TRUE,
      need_to_run = FALSE,
      last_evaluation_date = "2020-01-01",
      stringsAsFactors = FALSE
    )

  resolve_model_rerun_flag(
    data_source = data_source,
    sel_region = "Europe",
    sel_climatezone = "Temperate",
    sel_variable = "temp_annual",
    advance_seed = FALSE
  )

  files_saved <-
    list.files(
      path = file.path(temp_storage_root, "Temporal_models"),
      pattern = "general_model_config_table",
      full.names = FALSE
    )

  testthat::expect_true(length(files_saved) >= 1L)
})

testthat::test_that("resolve_model_rerun_flag() validates required columns", {
  testthat::skip_if_not_installed("RUtilpol")

  data_source <-
    data.frame(
      region = "Europe",
      climatezone = "Temperate",
      variable = "temp_annual",
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    resolve_model_rerun_flag(
      data_source = data_source,
      sel_region = "Europe",
      sel_climatezone = "Temperate",
      sel_variable = "temp_annual",
      advance_seed = FALSE
    ),
    regexp = "required model config columns"
  )
})

testthat::test_that("resolve_model_rerun_flag() supports model_id configs", {
  data_source <-
    data.frame(
      model_id = c("pap_temporal__n0", "pap_temporal__roc"),
      need_to_be_evaluated = c(TRUE, TRUE),
      need_to_run = c(FALSE, FALSE),
      last_evaluation_date = c("2020-01-01", "2020-01-01"),
      stringsAsFactors = FALSE
    )

  result <-
    resolve_model_rerun_flag(
      data_source = data_source,
      sel_model_id = "pap_temporal__n0",
      save_table = FALSE,
      advance_seed = FALSE
    )

  testthat::expect_identical(
    result[["need_to_be_evaluated"]],
    c(FALSE, TRUE)
  )
  testthat::expect_identical(result[["need_to_run"]], c(TRUE, FALSE))
  testthat::expect_equal(
    as.Date(result[["last_evaluation_date"]][1]),
    Sys.Date()
  )
})

testthat::test_that("resolve_model_rerun_flag() advances the selected seed", {
  data_source <-
    data.frame(
      model_id = c("pap_temporal__n0", "pap_temporal__roc"),
      need_to_be_evaluated = c(TRUE, TRUE),
      need_to_run = c(FALSE, FALSE),
      last_evaluation_date = c("2020-01-01", "2020-01-01"),
      seed_base = c(1234L, 1234L),
      seed_attempt = c(1L, 1L),
      sampling_seed = c(101L, 102L),
      seed_change_reason = "initial_model_seed",
      stringsAsFactors = FALSE
    )

  result <-
    resolve_model_rerun_flag(
      data_source = data_source,
      sel_model_id = "pap_temporal__n0",
      save_table = FALSE
    )

  testthat::expect_identical(result[["seed_attempt"]], c(2L, 1L))
  testthat::expect_false(result[["sampling_seed"]][1] == 101L)
  testthat::expect_identical(
    result[["seed_change_reason"]][1],
    "manual_rerun"
  )
})
