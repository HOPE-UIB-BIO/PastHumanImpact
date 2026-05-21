testthat::test_that("flag_model_to_rerun() updates selected row and saves table", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <-
    file.path(tempdir(), "phi-model-rerun")

  temp_storage_root_unix <-
    gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Predictor_models"),
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

  flag_model_to_rerun(
    data_source = data_source,
    sel_region = "Europe",
    sel_climatezone = "Temperate",
    sel_variable = "temp_annual"
  )

  saved <-
    RUtilpol::get_latest_file(
      file_name = "predictor_models_config_table",
      dir = paste0(temp_storage_root_unix, "/Predictor_models"),
      verbose = FALSE
    )

  testthat::expect_identical(saved[["need_to_be_evaluated"]], c(FALSE, TRUE))
  testthat::expect_identical(saved[["need_to_run"]], c(TRUE, FALSE))
  testthat::expect_equal(as.Date(saved[["last_evaluation_date"]][1]), Sys.Date())
})

testthat::test_that("flag_model_to_rerun() leaves unmatched rows unchanged", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <-
    file.path(tempdir(), "phi-model-rerun-unmatched")
  temp_storage_root_unix <-
    gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Predictor_models"),
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

  flag_model_to_rerun(
    data_source = data_source,
    sel_region = "Oceania",
    sel_climatezone = "Arid",
    sel_variable = "prec_annual"
  )

  saved <-
    RUtilpol::get_latest_file(
      file_name = "predictor_models_config_table",
      dir = paste0(temp_storage_root_unix, "/Predictor_models"),
      verbose = FALSE
    )

  testthat::expect_identical(saved[["need_to_be_evaluated"]], c(TRUE, TRUE))
  testthat::expect_identical(saved[["need_to_run"]], c(FALSE, FALSE))
  testthat::expect_identical(as.character(saved[["last_evaluation_date"]]), c("2020-01-01", "2020-01-01"))
})

testthat::test_that("flag_model_to_rerun() writes one saved table", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <-
    file.path(tempdir(), "phi-model-rerun-files")
  temp_storage_root_unix <-
    gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Predictor_models"),
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

  flag_model_to_rerun(
    data_source = data_source,
    sel_region = "Europe",
    sel_climatezone = "Temperate",
    sel_variable = "temp_annual"
  )

  files_saved <-
    list.files(
      path = file.path(temp_storage_root, "Predictor_models"),
      pattern = "predictor_models_config_table",
      full.names = FALSE
    )

  testthat::expect_true(length(files_saved) >= 1L)
})