run_project_tests <- function(
  test_dir = here::here("R", "tests", "testthat"),
  pattern = "^test-.*\\.R$",
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
) {
  is_absolute_path <- function(path_input) {
    grepl("^[A-Za-z]:[/\\\\]|^/|^\\\\\\\\", path_input)
  }

  if (
    isFALSE(is.character(test_dir)) || length(test_dir) != 1
  ) {
    stop("`test_dir` must be a single directory path.", call. = FALSE)
  }

  test_dir_resolved <-
    if (is_absolute_path(test_dir)) {
      test_dir
    } else {
      here::here(test_dir)
    }

  if (
    isFALSE(dir.exists(test_dir_resolved))
  ) {
    stop("`path` must point to an existing directory.", call. = FALSE)
  }

  source(
    here::here("R", "00_Config_file.R")
  )

  testthat::test_dir(
    path = test_dir_resolved,
    pattern = pattern,
    stop_on_failure = stop_on_failure,
    stop_on_warning = stop_on_warning
  )

  invisible(NULL)
}
