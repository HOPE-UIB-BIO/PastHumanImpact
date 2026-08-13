#' @title Build a plot to one grob row
#' @description Add a ggplot grob to the first column of a selected gtable row.
#' @param current_grob Existing gtable object.
#' @param map_plot ggplot object to insert.
#' @param panel_row Numeric target row.
#' @return The updated gtable object.
#' @examples
#' \dontrun{
#' build_plot_grob_row(layout_grob, map_plot, 3L)
#' }
build_plot_grob_row <- function(current_grob, map_plot, panel_row) {
  assertthat::assert_that(
    inherits(current_grob, "gtable"),
    inherits(map_plot, "ggplot"),
    is.numeric(panel_row),
    length(panel_row) == 1L,
    is.finite(panel_row),
    msg = "Plot-grob row inputs do not satisfy the contract."
  )

  res_grob <-
    gtable::gtable_add_grob(
      x = current_grob,
      grobs = ggplot2::ggplotGrob(map_plot),
      t = panel_row,
      b = panel_row,
      l = 1,
      r = 1,
      clip = "on"
    )

  return(res_grob)
}
