#' @title Classify human-climate outcomes from 95 % intervals
#' @description
#' Assign the outcome class of the decision metric: human when the interval
#' of the human-minus-climate balance lies above zero, climate when it lies
#' below zero, and no clear dominance when it includes zero.
#' @param lwr
#' Numeric vector of lower interval bounds.
#' @param upr
#' Numeric vector of upper interval bounds, the same length as `lwr`.
#' @return
#' A character vector the length of `lwr` with values `"human"`,
#' `"climate"`, `"no_clear_dominance"`, or `NA` when a bound is not finite.
#' @details
#' An interval touching zero (a bound exactly zero) includes zero.
#' @examples
#' classify_human_climate_outcome(lwr = c(0.1, -0.3), upr = c(0.4, 0.2))
classify_human_climate_outcome <- function(
  lwr,
  upr
) {
  assertthat::assert_that(
    is.numeric(lwr) || all(is.na(lwr)),
    is.numeric(upr) || all(is.na(upr)),
    length(lwr) == length(upr),
    msg = "Interval bounds must be numeric vectors of equal length."
  )

  res <-
    dplyr::case_when(
      !is.finite(lwr) | !is.finite(upr) ~ NA_character_,
      lwr > 0 ~ "human",
      upr < 0 ~ "climate",
      .default = "no_clear_dominance"
    )

  return(res)
}
