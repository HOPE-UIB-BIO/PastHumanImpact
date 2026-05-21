run_project_tests <- function(
  test_dir = file.path("R", "tests", "testthat"),
  pattern = "^test-.*\\.R$",
  source_config = TRUE
) {
  if (
    isTRUE(source_config)
  ) {
    Sys.setenv(PHI_SKIP_RENV_RESTORE = "true")
    source(
      file.path("R", "00_Config_file.R")
    )
  }

  testthat::test_dir(
    path = test_dir,
    pattern = pattern
  )

  invisible(NULL)
}
