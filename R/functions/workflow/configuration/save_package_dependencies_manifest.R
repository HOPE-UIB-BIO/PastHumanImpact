#' @title Save the static renv dependency manifest
#' @description
#' Generate literal `library()` calls from the canonical package list so
#' `renv` can discover packages that the project loads dynamically.
#' The file is written only when its generated contents change.
#' @param vec_package_names
#' Character vector of package names in display order.
#' @param path_output_file
#' Path to the generated R manifest.
#' @return
#' Invisibly, `TRUE` when the file changed and `FALSE` otherwise.
save_package_dependencies_manifest <- function(
  vec_package_names,
  path_output_file
) {
  assertthat::assert_that(
    is.character(vec_package_names),
    length(vec_package_names) > 0L,
    isFALSE(anyNA(vec_package_names)),
    all(nzchar(vec_package_names)),
    msg = "`vec_package_names` must be a non-empty character vector."
  )

  assertthat::assert_that(
    is.character(path_output_file),
    length(path_output_file) == 1L,
    isFALSE(is.na(path_output_file)),
    nzchar(path_output_file),
    msg = "`path_output_file` must be a single non-empty path."
  )

  vec_invalid_package_names <-
    vec_package_names[
      !stringr::str_detect(
        string = vec_package_names,
        pattern = "^[A-Za-z][A-Za-z0-9.]*$"
      )
    ]

  assertthat::assert_that(
    length(vec_invalid_package_names) == 0L,
    msg = stringr::str_glue(
      "Invalid package name(s): ",
      "{stringr::str_c(vec_invalid_package_names, collapse = ', ')}."
    )
  )

  vec_package_names <-
    unique(vec_package_names)

  path_output_directory <-
    dirname(path_output_file)

  assertthat::assert_that(
    dir.exists(path_output_directory),
    msg = stringr::str_glue(
      "Output directory does not exist: {path_output_directory}"
    )
  )

  vec_generated_lines <-
    c(
      "# This file is generated from `package_list` in `R/00_Config_file.R`.",
      "# Do not edit it manually.",
      "# Literal library calls let renv discover dynamically loaded packages.",
      "if (",
      "  FALSE",
      ") {",
      stringr::str_c(
        "  library(",
        vec_package_names,
        ")"
      ),
      "}"
    )

  text_generated_contents <-
    stringr::str_c(
      stringr::str_c(
        vec_generated_lines,
        collapse = "\n"
      ),
      "\n"
    )

  text_current_contents <- NULL

  if (
    file.exists(path_output_file)
  ) {
    vec_current_lines <-
      readLines(
        con = path_output_file,
        warn = FALSE,
        encoding = "UTF-8"
      )

    text_current_contents <-
      stringr::str_c(
        stringr::str_c(
          vec_current_lines,
          collapse = "\n"
        ),
        "\n"
      )
  }

  if (
    identical(
      text_generated_contents,
      text_current_contents
    )
  ) {
    return(invisible(FALSE))
  }

  writeLines(
    text = vec_generated_lines,
    con = path_output_file,
    sep = "\n",
    useBytes = TRUE
  )

  return(invisible(TRUE))
}
