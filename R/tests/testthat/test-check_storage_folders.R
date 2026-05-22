testthat::test_that("check_storage_folders() accepts expected tree", {
  root_dir <-
    file.path(tempdir(), "storage-check-ok")

  expected_dirs <-
    c(
      "Assembly",
      "C14",
      "Climate",
      "Events",
      "Predictor_models",
      "Predictor_models/General_trends",
      "Spatial",
      "Spatial/Climatezones",
      "Spatial/Regions_shapefile",
      "SPD",
      "Targets_data",
      "Targets_data/analyses_h1",
      "Targets_data/analyses_h2",
      "Targets_data/pipeline_events",
      "Targets_data/pipeline_paps",
      "Targets_data/pipeline_pollen_data",
      "Targets_data/pipeline_predictors"
    )

  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)

  for (dir_name in expected_dirs) {
    dir.create(
      file.path(root_dir, dir_name),
      recursive = TRUE,
      showWarnings = FALSE
    )
  }

  testthat::expect_silent(check_storage_folders(root_dir))
})

testthat::test_that("check_storage_folders() errors when required folder is missing", {
  root_dir <-
    file.path(tempdir(), "storage-check-missing")

  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root_dir, "Assembly"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(root_dir, "C14"), recursive = TRUE, showWarnings = FALSE)

  testthat::expect_error(
    check_storage_folders(root_dir),
    regexp = "Data folder structure is not as expected"
  )
})

testthat::test_that("check_storage_folders() errors for non-existing root", {
  path_missing <-
    file.path(tempdir(), "does-not-exist", "nested")

  testthat::expect_error(
    check_storage_folders(path_missing),
    regexp = "must point to an existing directory"
  )
})

testthat::test_that("check_storage_folders() validates path input type", {
  testthat::expect_error(
    check_storage_folders(path = 123),
    regexp = "single character"
  )
})