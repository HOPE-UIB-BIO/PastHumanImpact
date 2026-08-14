#' @title Build the HVarPart density strip horizontally
#' @description
#' Change only the first bottom facet strip, representing density, to
#' horizontal text while retaining rotated climate-zone strips.
#' @param statistical_grob A gtable produced from the HVarPart plot.
#' @return The modified gtable.
#' @examples
#' \dontrun{
#' build_horizontal_hvarpart_density_strip(ggplot2::ggplotGrob(plot_input))
#' }
build_horizontal_hvarpart_density_strip <- function(statistical_grob) {
  assertthat::assert_that(
    inherits(statistical_grob, "gtable"),
    msg = "The statistical plot must be supplied as a gtable."
  )

  strip_index <-
    statistical_grob[["layout"]] |>
    dplyr::filter(.data[["name"]] == "strip-b-1") |>
    dplyr::pull(.data[["z"]])
  strip_position <-
    which(statistical_grob[["layout"]][["name"]] == "strip-b-1")
  if (
    length(strip_index) != 1L || length(strip_position) != 1L
  ) {
    cli::cli_abort("Could not identify the HVarPart density strip.")
  }

  strip_grob <- statistical_grob[["grobs"]][[strip_position]]
  strip_tree <- strip_grob[["grobs"]][[1]]
  title_position <-
    which(
      grepl(
        "^strip.text.x.bottom",
        names(strip_tree[["children"]])
      )
    )
  if (
    length(title_position) != 1L
  ) {
    cli::cli_abort("Could not identify the density-strip title grob.")
  }

  title_grob <- strip_tree[["children"]][[title_position]]
  text_grob <- title_grob[["children"]][[1]]
  text_grob[["rot"]] <- 0
  title_grob[["children"]][[1]] <- text_grob
  strip_tree[["children"]][[title_position]] <- title_grob
  strip_grob[["grobs"]][[1]] <- strip_tree
  statistical_grob[["grobs"]][[strip_position]] <- strip_grob

  return(statistical_grob)
}
