#' @title Calculate partial-RDA adjusted R-squared
#' @description
#' Fit an RDA for one explanatory matrix, optionally conditional on another
#' matrix, and return its adjusted R-squared.
#' @param response Numeric response matrix.
#' @param explanatory Numeric explanatory matrix.
#' @param conditioning Optional numeric conditioning matrix.
#' @return One numeric adjusted-R-squared value.
#' @examples
#' \dontrun{
#' calculate_partial_rda_adjusted_r_squared(
#'   response = community,
#'   explanatory = human,
#'   conditioning = climate
#' )
#' }
calculate_partial_rda_adjusted_r_squared <- function(
  response,
  explanatory,
  conditioning = NULL
) {
  assertthat::assert_that(
    is.matrix(response),
    is.matrix(explanatory),
    is.null(conditioning) || is.matrix(conditioning),
    nrow(response) == nrow(explanatory),
    is.null(conditioning) || nrow(response) == nrow(conditioning),
    msg = "Partial RDA inputs do not satisfy the required contract."
  )

  model_rda <-
    vegan::rda(
      X = response,
      Y = explanatory,
      Z = conditioning,
      scale = TRUE
    )
  res_adjusted_r_squared <-
    vegan::RsquareAdj(model_rda)[["adj.r.squared"]]

  return(res_adjusted_r_squared)
}
