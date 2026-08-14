#' @title Build the HVarPart density-facet divider
#' @description
#' Add a right-side divider to each density panel without adding borders
#' around the climate-zone panels.
#' @param statistical_grob A gtable produced from the HVarPart plot.
#' @param divider_colour Colour used for the divider.
#' @param divider_linewidth Divider width in ggplot linewidth units.
#' @return The modified gtable.
#' @examples
#' \dontrun{
#' build_hvarpart_density_divider(ggplot2::ggplotGrob(plot_input))
#' }
build_hvarpart_density_divider <- function(
  statistical_grob,
  divider_colour = common_gray,
  divider_linewidth = line_size
) {
  assertthat::assert_that(
    inherits(statistical_grob, "gtable"),
    assertthat::is.string(divider_colour),
    is.numeric(divider_linewidth),
    length(divider_linewidth) == 1L,
    is.finite(divider_linewidth),
    divider_linewidth > 0,
    msg = "Density-divider inputs do not satisfy the contract."
  )

  data_panels <-
    statistical_grob[["layout"]] |>
    dplyr::filter(grepl("^panel-", .data[["name"]]))
  density_column <- min(data_panels[["l"]])
  data_density_panels <-
    data_panels |>
    dplyr::filter(.data[["l"]] == density_column) |>
    dplyr::arrange(.data[["t"]])
  if (
    nrow(data_density_panels) == 0L
  ) {
    cli::cli_abort("Could not identify the HVarPart density panels.")
  }

  divider_grob <-
    grid::segmentsGrob(
      x0 = grid::unit(1, "npc"),
      x1 = grid::unit(1, "npc"),
      y0 = grid::unit(0, "npc"),
      y1 = grid::unit(1, "npc"),
      gp = grid::gpar(
        col = divider_colour,
        lwd = divider_linewidth * ggplot2::.pt
      )
    )
  result <-
    purrr::reduce2(
      .x = data_density_panels[["t"]],
      .y = data_density_panels[["l"]],
      .init = statistical_grob,
      .f = ~ gtable::gtable_add_grob(
        x = .x,
        grobs = divider_grob,
        t = .y,
        b = .y,
        l = ..3,
        r = ..3,
        z = Inf,
        clip = "off",
        name = "density-divider"
      )
    )

  return(result)
}
