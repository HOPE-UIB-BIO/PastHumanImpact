#' @title Build the untruncated importance fill scale
#' @description Build the bounded presentation colour scale used to display
#' untruncated hierarchical contributions.
#' @param palette Character vector of at least three colours.
#' @param guide Legend guide specification passed to ggplot2.
#' @return A ggplot2 continuous fill scale.
#' @examples
#' build_untruncated_importance_fill_scale(c("white", "pink", "red"))
build_untruncated_importance_fill_scale <- function(
  palette,
  guide = "none"
) {
  assertthat::assert_that(
    is.character(palette),
    length(palette) >= 3L,
    msg = "The untruncated importance scale requires three colours."
  )

  res_scale <-
    ggplot2::scale_fill_gradientn(
      "Untruncated signed human hierarchical contribution",
      colours = palette,
      values = c(0, 0.5, 1),
      limits = c(0, 1),
      oob = scales::squish,
      breaks = c(0, 0.5, 1),
      labels = c("\u2264 0", "0.5", "\u2265 1"),
      guide = guide
    )

  return(res_scale)
}
