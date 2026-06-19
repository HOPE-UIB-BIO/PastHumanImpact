#' @title Get default family key for temporal model variables
#' @description
#' Return the default modelling family key for predictor, event, and PAP
#' variables.
#' @param variable Character scalar model variable.
#' @param analysis Character scalar model analysis.
#' @return Character scalar family key.
#' @examples
#' \dontrun{
#' family_key <- get_default_model_family_key("n0", "pap_temporal")
#' }
get_default_model_family_key <- function(variable, analysis) {
  assertthat::assert_that(
    is.character(variable),
    length(variable) == 1,
    is.character(analysis),
    length(analysis) == 1,
    msg = "`variable` and `analysis` must be character scalars."
  )

  res_family_key <-
    dplyr::case_when(
      analysis == "event_temporal" ~ "bernoulli_logit",
      variable %in% c("spd", "prec_summer", "prec_win") ~ "hurdle_gamma_log",
      variable %in% c("temp_annual", "temp_cold") ~ "gaussian_identity",
      analysis == "pap_temporal" ~ "student_identity",
      .default = "student_identity"
    )

  return(res_family_key)
}
