check_storage_folders <- function(path) {
  expected_folders <- c(
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
}
