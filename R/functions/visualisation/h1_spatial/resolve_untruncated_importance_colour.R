#' @title Map untruncated importance values to colours
#' @description
#' Clamp values to the presentation domain and map them through a continuous
#' colour palette.
#' @param values Numeric vector of importance values.
#' @param palette Character vector of colours.
#' @return Character vector of colours with the same length as `values`.
#' @examples
#' resolve_untruncated_importance_colour(c(-1, 0.5, 2), c("white", "red"))
resolve_untruncated_importance_colour <- function(values, palette) {
  assertthat::assert_that(
    is.numeric(values),
    is.character(palette),
    length(palette) >= 2L,
    msg = "Importance colours require numeric values and a colour palette."
  )

  res_colours <-
    scales::col_numeric(
      palette = palette,
      domain = c(0, 1)
    )(
      pmin(pmax(values, 0), 1)
    )

  return(res_colours)
}
