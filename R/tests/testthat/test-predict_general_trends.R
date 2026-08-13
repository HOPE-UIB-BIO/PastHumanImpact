testthat::test_that("predict_general_trends returns empty for empty predictors input", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <- file.path(tempdir(), "phi-general-trends-empty-pred")
  temp_storage_root_unix <- gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Temporal_models", "General_trends"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  had_data_storage_path <- exists("data_storage_path", inherits = TRUE)

  if (had_data_storage_path) {
    old_data_storage_path <- get("data_storage_path", envir = .GlobalEnv)
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
      analysis = character(0),
      model_id = character(0),
      variable = character(0),
      stringsAsFactors = FALSE
    )

  result <-
    predict_general_trends(
      data_source = data_source,
      sel_type = "predictors"
    )

  testthat::expect_identical(nrow(result), 0L)
})

testthat::test_that("predict_general_trends returns empty for empty events input", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <- file.path(tempdir(), "phi-general-trends-empty-events")
  temp_storage_root_unix <- gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Temporal_models", "General_trends"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  had_data_storage_path <- exists("data_storage_path", inherits = TRUE)

  if (had_data_storage_path) {
    old_data_storage_path <- get("data_storage_path", envir = .GlobalEnv)
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
      analysis = character(0),
      model_id = character(0),
      variable = character(0),
      stringsAsFactors = FALSE
    )

  result <-
    predict_general_trends(
      data_source = data_source,
      sel_type = "events"
    )

  testthat::expect_identical(nrow(result), 0L)
})

testthat::test_that("predict_general_trends validates required columns", {
  bad_source <-
    data.frame(
      analysis = character(0),
      stringsAsFactors = FALSE
    )

  testthat::expect_error(
    predict_general_trends(
      data_source = bad_source,
      sel_type = "events"
    ),
    regexp = "variable"
  )
})

testthat::test_that("predict_general_trends filters combined predictions by analysis", {
  testthat::skip_if_not_installed("RUtilpol")

  temp_storage_root <- file.path(tempdir(), "phi-general-trends-combined")
  temp_storage_root_unix <- gsub("\\\\", "/", temp_storage_root)

  dir.create(
    file.path(temp_storage_root, "Temporal_models", "General_trends"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  had_data_storage_path <- exists("data_storage_path", inherits = TRUE)

  if (had_data_storage_path) {
    old_data_storage_path <- get("data_storage_path", envir = .GlobalEnv)
  }

  assign("data_storage_path", paste0(temp_storage_root_unix, "/"), envir = .GlobalEnv)

  on.exit({
    if (had_data_storage_path) {
      assign("data_storage_path", old_data_storage_path, envir = .GlobalEnv)
    } else if (exists("data_storage_path", envir = .GlobalEnv, inherits = FALSE)) {
      rm("data_storage_path", envir = .GlobalEnv)
    }
  }, add = TRUE)

  data_predictions <-
    data.frame(
      analysis = c("predictor_temporal", "pap_temporal"),
      model_id = c("predictor_temporal__spd", "pap_temporal__n0"),
      variable = c("spd", "n0"),
      age = c(0, 0),
      estimate = c(1, 2),
      stringsAsFactors = FALSE
    )

  RUtilpol::save_latest_file(
    object_to_save = data_predictions,
    file_name = "general_temporal_model_predictions",
    dir = file.path(temp_storage_root_unix, "Temporal_models", "General_trends"),
    prefered_format = "csv",
    verbose = FALSE
  )

  data_source <-
    data.frame(
      analysis = c("predictor_temporal", "pap_temporal"),
      model_id = c("predictor_temporal__spd", "pap_temporal__n0"),
      variable = c("spd", "n0"),
      stringsAsFactors = FALSE
    )

  result <-
    predict_general_trends(
      data_source = data_source,
      sel_type = "paps"
    )

  testthat::expect_identical(result[["model_id"]], "pap_temporal__n0")
  testthat::expect_identical(result[["value"]], 2)
})
