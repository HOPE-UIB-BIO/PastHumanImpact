#' @title Save one diagnostic figure in standard formats
#' @description
#' Save a diagnostic plot as PNG and PDF with a shared physical size.
#' @param fig_object Plot object accepted by ggplot2::ggsave.
#' @param fig_name Character scalar output basename.
#' @param path_figures Existing output directory.
#' @param width Numeric width.
#' @param height Numeric height.
#' @param units Character scalar size unit.
#' @return Invisibly returns the written file paths.
#' @examples
#' \dontrun{
#' save_diagnostic_figure_formats(
#'   fig_object = ggplot2::ggplot(),
#'   fig_name = "diagnostic",
#'   path_figures = tempdir(),
#'   width = 100,
#'   height = 100,
#'   units = "mm"
#' )
#' }
save_diagnostic_figure_formats <- function(
  fig_object,
  fig_name,
  path_figures,
  width,
  height,
  units
) {
  assertthat::assert_that(
    is.character(fig_name),
    length(fig_name) == 1L,
    !is.na(fig_name),
    nzchar(fig_name),
    dir.exists(path_figures),
    is.numeric(width),
    length(width) == 1L,
    is.numeric(height),
    length(height) == 1L,
    is.character(units),
    length(units) == 1L,
    msg = "Diagnostic figure save inputs are invalid."
  )

  output_paths <-
    file.path(
      path_figures,
      paste0(fig_name, ".", c("png", "pdf"))
    )

  purrr::walk(
    .x = output_paths,
    .f = ~ ggplot2::ggsave(
      filename = .x,
      plot = fig_object,
      width = width,
      height = height,
      units = units,
      bg = "white"
    )
  )

  return(invisible(output_paths))
}
