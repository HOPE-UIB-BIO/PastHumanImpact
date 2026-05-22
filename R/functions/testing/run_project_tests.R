run_project_tests <- function(
  test_dir = file.path("R", "tests", "testthat"),
  pattern = "^test-.*\\.R$"
) {
  source(
    file.path("R", "00_Config_file.R")
  )

  testthat::test_dir(
    path = test_dir,
    pattern = pattern
  )

  invisible(NULL)
}
