#' @title Render a reporting document
#' @description
#' Render one Quarto reporting source and return its expected output path.
#' @param input Existing Quarto source path.
#' @param output_file Optional output filename.
#' @return Normalized path to the rendered document.
#' @examples
#' \dontrun{
#' render_reporting_document("report.qmd")
#' }
render_reporting_document <- function(
  input,
  output_file = NULL
) {
  assertthat::assert_that(
    is.character(input),
    length(input) == 1L,
    file.exists(input),
    is.null(output_file) ||
      (
        is.character(output_file) &&
          length(output_file) == 1L &&
          !is.na(output_file) &&
          nzchar(output_file)
      ),
    msg = "Reporting-document inputs are invalid."
  )

  quarto::quarto_render(
    input = input,
    output_file = output_file,
    quiet = TRUE
  )

  output_name <-
    if (
      is.null(output_file)
    ) {
      stringr::str_replace(
        basename(input),
        "[.]qmd$",
        ".html"
      )
    } else {
      output_file
    }

  output_path <-
    file.path(dirname(input), output_name)

  assertthat::assert_that(
    file.exists(output_path),
    msg = "The reporting document did not produce its expected output."
  )

  res_path <-
    normalizePath(
      output_path,
      winslash = "/",
      mustWork = TRUE
    )

  return(res_path)
}
