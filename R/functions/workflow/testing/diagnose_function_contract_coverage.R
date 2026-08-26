diagnose_function_contract_coverage <- function(
  function_dir = here::here("R", "functions"),
  test_dir = here::here("R", "tests", "testthat"),
  fail_on_missing = FALSE
) {
  if (
    isFALSE(is.character(function_dir)) || length(function_dir) != 1
  ) {
    stop("`function_dir` must be a single directory path.", call. = FALSE)
  }

  if (
    isFALSE(is.character(test_dir)) || length(test_dir) != 1
  ) {
    stop("`test_dir` must be a single directory path.", call. = FALSE)
  }

  function_dir_resolved <-
    if (is_absolute_project_path(function_dir)) {
      function_dir
    } else {
      here::here(function_dir)
    }

  test_dir_resolved <-
    if (is_absolute_project_path(test_dir)) {
      test_dir
    } else {
      here::here(test_dir)
    }

  if (
    isFALSE(dir.exists(function_dir_resolved))
  ) {
    stop("`function_dir` does not exist.", call. = FALSE)
  }

  if (
    isFALSE(dir.exists(test_dir_resolved))
  ) {
    stop("`test_dir` does not exist.", call. = FALSE)
  }

  function_files_all <-
    list.files(
      path = function_dir_resolved,
      pattern = "\\.R$",
      recursive = TRUE,
      full.names = TRUE
    )

  # Exclude test runner helper functions from contract enforcement.
  function_files_non_testing <-
    function_files_all[
      !grepl(
        "/testing/",
        gsub("\\\\", "/", function_files_all)
      )
    ]

  if (length(function_files_non_testing) == 0) {
    res <-
      data.frame(
        function_name = character(),
        has_roxygen = logical(),
        has_validation = logical(),
        has_argument_assertion = logical(),
        has_test_file = logical(),
        stringsAsFactors = FALSE
      )

    return(invisible(res))
  }

  coverage_table <-
    function_files_non_testing |>
    purrr::map(
      .f = ~ {
        path_file <- .x

        lines <-
          readLines(
            con = path_file,
            warn = FALSE,
            encoding = "UTF-8"
          )

        function_name <-
          sub("\\.R$", "", basename(path_file))

        test_file <-
          file.path(test_dir_resolved, paste0("test-", function_name, ".R"))

        data.frame(
          function_name = function_name,
          has_roxygen = any(grepl("^#'", lines, useBytes = TRUE)),
          has_validation = any(grepl(
            "assertthat::assert_that\\(",
            lines,
            useBytes = TRUE
          )),
          has_argument_assertion = any(grepl(
            "assertthat::assert_that\\(",
            lines,
            useBytes = TRUE
          )),
          has_test_file = file.exists(test_file),
          stringsAsFactors = FALSE
        )
      }
    ) |>
    dplyr::bind_rows()

  coverage_table[["has_validation"]] <-
    coverage_table[["has_argument_assertion"]]

  missing_contract <-
    coverage_table[
      !coverage_table$has_roxygen |
        !coverage_table$has_argument_assertion |
        !coverage_table$has_test_file, ,
      drop = FALSE
    ]

  if (nrow(missing_contract) > 0) {
    message(
      "Contract coverage review: ",
      nrow(missing_contract),
      " function(s) missing roxygen, argument assertion, or tests."
    )

    message(
      paste(
        purrr::pmap_chr(
          .l = missing_contract,
          .f = ~ paste0(
            " - ",
            ..1,
            " (roxygen=", ..2,
            ", validation=", ..3,
            ", argument_assertion=", ..4,
            ", tests=", ..5,
            ")"
          )
        ),
        collapse = "\n"
      )
    )

    if (isTRUE(fail_on_missing)) {
      stop(
        "Contract coverage gate failed.",
        call. = FALSE
      )
    }
  } else {
    message(
      "Contract coverage review: all checked functions have roxygen, ",
      "argument assertion, and tests."
    )
  }

  return(invisible(coverage_table))
}
