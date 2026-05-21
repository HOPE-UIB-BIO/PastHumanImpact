run_test_file <- function(
  test_file,
  source_config = TRUE
) {
  if (
    isFALSE(is.character(test_file)) || length(test_file) != 1
  ) {
    stop("`test_file` must be a single file path.", call. = FALSE)
  }

  if (
    isFALSE(file.exists(test_file))
  ) {
    stop("`test_file` does not exist.", call. = FALSE)
  }

  if (
    isTRUE(source_config)
  ) {
    Sys.setenv(PHI_SKIP_RENV_RESTORE = "true")
    source(
      file.path("R", "00_Config_file.R")
    )
  }

  testthat::test_file(
    path = test_file
  )

  invisible(NULL)
}