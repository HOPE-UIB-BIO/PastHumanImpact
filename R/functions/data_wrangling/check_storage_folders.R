#' @title Validate Data Storage Folder Structure
#' @description
#' Validate that the expected project data folder tree exists under a supplied
#' root directory.
#' @param path Character scalar path to the data storage root directory.
#' @param create_missing Logical. If `TRUE`, create missing expected
#' subdirectories before validating the folder tree.
#' @return
#' Invisibly returns `NULL` when all required folders are present; otherwise
#' throws an error.
check_storage_folders <- function(path, create_missing = FALSE) {
  assertthat::assert_that(
    is.character(path),
    length(path) == 1,
    msg = "`path` must be a single character value."
  )
  assertthat::assert_that(
    dir.exists(path),
    msg = "`path` must point to an existing directory."
  )
  assertthat::assert_that(
    is.logical(create_missing),
    length(create_missing) == 1,
    msg = "`create_missing` must be a single logical value."
  )

  expected_folders <- c(
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
    "Targets_data/analyses_h1",
    "Targets_data/analyses_h2",
    "Targets_data/pipeline_events",
    "Targets_data/pipeline_paps",
    "Targets_data/pipeline_pollen_data",
    "Targets_data/pipeline_predictors"
  )

  if (
    isTRUE(create_missing)
  ) {
    purrr::walk(
      .x = expected_folders,
      .f = ~ dir.create(
        path = file.path(path, .x),
        recursive = TRUE,
        showWarnings = FALSE
      )
    )
  }

  assertthat::assert_that(
    all(expected_folders %in% list.dirs(
      path = path,
      recursive = TRUE,
      full.names = FALSE
    )),
    msg = paste(
      "The Data folder structure is not as expected",
      "Please check the README file and `R/00_Config_file.R` (section 4)."
    )
  )

  return(invisible(NULL))
}
