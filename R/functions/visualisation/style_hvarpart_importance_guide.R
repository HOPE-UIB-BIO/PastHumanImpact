#' @title Style an HVarPart importance guide
#' @description
#' Restores the vertical scale and label on an inset-style HVarPart plot so it
#' can be exported as a standalone figure guide.
#' @param plot HVarPart inset ggplot.
#' @param y_label Vertical-axis label.
#' @return A ggplot object with guide axes enabled.
#' @examples
#' \dontrun{
#' style_hvarpart_importance_guide(inset, "Signed allocation")
#' }
style_hvarpart_importance_guide <- function(plot, y_label) {
  assertthat::assert_that(
    inherits(plot, "ggplot"),
    assertthat::is.string(y_label),
    msg = "Importance guide inputs must be a ggplot and one label."
  )

  result <-
    plot +
    ggplot2::theme(
      axis.line.y = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      ),
      axis.text.y = ggplot2::element_text(
        colour = common_gray,
        size = text_size
      ),
      axis.title.y = ggplot2::element_text(
        colour = common_gray,
        size = text_size,
        angle = 90
      ),
      axis.ticks.y = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      ),
      panel.grid.major.y = ggplot2::element_line(
        colour = common_gray,
        linewidth = line_size
      ),
      plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
    ) +
    ggplot2::labs(y = y_label)

  return(result)
}
