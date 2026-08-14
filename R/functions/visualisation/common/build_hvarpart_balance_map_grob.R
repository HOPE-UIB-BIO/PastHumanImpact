#' @title Build a map to an HVarPart balance grob
#' @description Add one regional map to its matching statistical facet row.
#' @param current_grob Existing gtable object.
#' @param map_plot Regional ggplot map.
#' @param panel_row Integer target row in the gtable.
#' @param border_colour Colour of the equal-sized map-cell border.
#' @param border_linewidth Border width in ggplot2 linewidth units.
#' @return The gtable with the map added.
#' @examples
#' \dontrun{
#' build_hvarpart_balance_map_grob(layout_grob, map_plot, 3L)
#' }
build_hvarpart_balance_map_grob <- function(
  current_grob,
  map_plot,
  panel_row,
  border_colour = common_gray,
  border_linewidth = line_size
) {
  assertthat::assert_that(
    inherits(current_grob, "gtable"),
    inherits(map_plot, "ggplot"),
    is.numeric(panel_row),
    length(panel_row) == 1L,
    is.finite(panel_row),
    assertthat::is.string(border_colour),
    is.numeric(border_linewidth),
    length(border_linewidth) == 1L,
    is.finite(border_linewidth),
    border_linewidth > 0,
    msg = "HVarPart map-grob inputs do not satisfy the contract."
  )

  result <-
    gtable::gtable_add_grob(
      x = current_grob,
      grobs = ggplot2::ggplotGrob(map_plot),
      t = panel_row,
      b = panel_row,
      l = 1,
      r = 1,
      clip = "on"
    )
  result <-
    gtable::gtable_add_grob(
      x = result,
      grobs = grid::rectGrob(
        gp = grid::gpar(
          fill = NA,
          col = border_colour,
          lwd = border_linewidth * ggplot2::.pt
        )
      ),
      t = panel_row,
      b = panel_row,
      l = 1,
      r = 1,
      z = Inf,
      clip = "off",
      name = "map-frame"
    )

  return(result)
}
