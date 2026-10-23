#' @title Run the project test suite
#' @description
#' Run the testthat suite in a fresh R subprocess so results do not depend on
#' the state of the calling console.
#' @param test_dir
#' Single directory path containing the test files, absolute or relative to
#' the project root.
#' @param pattern
#' Regular expression selecting the test files.
#' @param stop_on_failure
#' Logical. If `TRUE`, abort when any test fails.
#' @param stop_on_warning
#' Logical. If `TRUE`, abort when any test warns.
#' @return
#' Invisibly returns `NULL`. Test results are printed to the console.
#' @details
#' Interactive consoles change console width, colours, the graphics device and
#' file encoding (e.g. radian sets `options(encoding = "UTF-8")` even under a
#' non-UTF-8 code page). These leak into expectations, so the suite always runs
#' via `callr::r()`, which matches a clean `Rscript` run.
run_project_tests <- function(
  test_dir = here::here("R", "tests", "testthat"),
  pattern = "^test-.*\\.R$",
  stop_on_failure = FALSE,
  stop_on_warning = FALSE
) {
  if (
    isFALSE(is.character(test_dir)) || length(test_dir) != 1
  ) {
    stop("`test_dir` must be a single directory path.", call. = FALSE)
  }

  test_dir_resolved <-
    if (is_absolute_project_path(test_dir)) {
      test_dir
    } else {
      here::here(test_dir)
    }

  if (
    isFALSE(dir.exists(test_dir_resolved))
  ) {
    stop("`path` must point to an existing directory.", call. = FALSE)
  }

  callr::r(
    func = function(
      path_config,
      path_tests,
      pattern,
      stop_on_failure,
      stop_on_warning
    ) {
      base::source(
        file = path_config
      )

      testthat::test_dir(
        path = path_tests,
        pattern = pattern,
        stop_on_failure = stop_on_failure,
        stop_on_warning = stop_on_warning
      )

      return(invisible(NULL))
    },
    args = list(
      path_config = here::here("R", "00_Config_file.R"),
      path_tests = test_dir_resolved,
      pattern = pattern,
      stop_on_failure = stop_on_failure,
      stop_on_warning = stop_on_warning
    ),
    wd = here::here(),
    show = TRUE,
    spinner = FALSE
  )

  invisible(NULL)
}
