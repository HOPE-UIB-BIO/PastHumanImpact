#' @title Save collinearity-managed sequence overview figures
#' @description Save the paired adjusted R-squared and predictor-selection
#' diagnostic figures as PNG and PDF files.
#' @param plot_r2 Paired sequence adjusted R-squared plot.
#' @param plot_selection Predictor-selection heatmap.
#' @param path_r2 R-squared figure path without extension.
#' @param path_selection Selection figure path without extension.
#' @return Character vector of the four written file paths.
#' @examples
#' \dontrun{save_collinearity_managed_overview_figures(p1, p2, "r2", "selection")}
save_collinearity_managed_overview_figures <- function(
  plot_r2,
  plot_selection,
  path_r2,
  path_selection
) {
  assertthat::assert_that(
    inherits(plot_r2, "ggplot"), inherits(plot_selection, "ggplot"),
    is.character(path_r2), length(path_r2) == 1L,
    is.character(path_selection), length(path_selection) == 1L,
    msg = "Sequence-overview figure outputs do not satisfy the contract."
  )
  paths <- c(
    paste0(path_r2, c(".png", ".pdf")),
    paste0(path_selection, c(".png", ".pdf"))
  )
  purrr::walk(unique(dirname(paths)), dir.create, recursive = TRUE,
              showWarnings = FALSE)
  ggplot2::ggsave(
    filename = paths[[1]], plot = plot_r2,
    width = 10, height = 8, units = "in", dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    filename = paths[[2]], plot = plot_r2,
    width = 10, height = 8, units = "in", bg = "white",
    device = grDevices::cairo_pdf
  )
  ggplot2::ggsave(
    filename = paths[[3]], plot = plot_selection,
    width = 12, height = 9.5, units = "in", dpi = 300, bg = "white"
  )
  ggplot2::ggsave(
    filename = paths[[4]], plot = plot_selection,
    width = 12, height = 9.5, units = "in", bg = "white",
    device = grDevices::cairo_pdf
  )
  return(paths)
}
