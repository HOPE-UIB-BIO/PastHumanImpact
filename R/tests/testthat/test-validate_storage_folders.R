testthat::test_that("validate_storage_folders() accepts expected tree", {
  root_dir <-
    file.path(tempdir(), "storage-check-ok")

  expected_dirs <-
    c(
      "Assembly",
      "C14",
      "Climate",
      "Events",
      "Temporal_models",
      "Temporal_models/General_trends",
      "Spatial",
      "Spatial/Climatezones",
      "Spatial/Regions_shapefile",
      "SPD",
      "Targets_data",
      "Targets_data/data_preparation",
      "Targets_data/analyses_h1",
      "Targets_data/temporal_models",
      "Targets_data/analyses_h2",
      "Targets_data/sensitivity_analyses"
    )

  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)

  purrr::walk(
    .x = expected_dirs,
    .f = ~ dir.create(
      file.path(root_dir, .x),
      recursive = TRUE,
      showWarnings = FALSE
    )
  )

  testthat::expect_silent(validate_storage_folders(root_dir))
})

testthat::test_that(
  "validate_storage_folders() errors when a required folder is missing",
  {
  root_dir <-
    file.path(tempdir(), "storage-check-missing")

  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(
    file.path(root_dir, "Assembly"),
    recursive = TRUE,
    showWarnings = FALSE
  )
  dir.create(
    file.path(root_dir, "C14"),
    recursive = TRUE,
    showWarnings = FALSE
  )

  testthat::expect_error(
    validate_storage_folders(root_dir),
    regexp = "Data folder structure is not as expected"
  )
  }
)

testthat::test_that(
  "validate_storage_folders() can create missing expected folders",
  {
  root_dir <-
    file.path(tempdir(), "storage-check-create")

  dir.create(root_dir, recursive = TRUE, showWarnings = FALSE)

  testthat::expect_silent(
    validate_storage_folders(root_dir, create_missing = TRUE)
  )

  testthat::expect_true(
    dir.exists(file.path(root_dir, "Temporal_models", "General_trends"))
  )
  }
)

testthat::test_that(
  "validate_storage_folders() errors for a non-existing root",
  {
  path_missing <-
    file.path(tempdir(), "does-not-exist", "nested")

  testthat::expect_error(
    validate_storage_folders(path_missing),
    regexp = "must point to an existing directory"
  )
  }
)

testthat::test_that("validate_storage_folders() validates path input type", {
  testthat::expect_error(
    validate_storage_folders(path = 123),
    regexp = "single character"
  )
})

testthat::test_that(
  "validate_storage_folders() validates create_missing input type",
  {
  testthat::expect_error(
    validate_storage_folders(path = tempdir(), create_missing = "yes"),
    regexp = "single logical"
  )
  }
)
