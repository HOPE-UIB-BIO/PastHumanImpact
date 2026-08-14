run_test_file <- function(
  test_file
) {
  if (
    isFALSE(is.character(test_file)) || length(test_file) != 1
  ) {
    stop("`test_file` must be a single file path.", call. = FALSE)
  }

  test_file_resolved <-
    if (is_absolute_project_path(test_file)) {
      test_file
    } else {
      here::here(test_file)
    }

  if (
    isFALSE(file.exists(test_file_resolved))
  ) {
    stop("`test_file` does not exist.", call. = FALSE)
  }

  source(
    here::here("R", "00_Config_file.R")
  )

  testthat::test_file(
    path = test_file_resolved
  )

  invisible(NULL)
}
