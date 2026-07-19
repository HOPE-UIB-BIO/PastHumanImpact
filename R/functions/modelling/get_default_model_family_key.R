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
      variable %in% c("n0", "n1", "n2", "roc") ~ "gamma_log",
      variable == "n1_minus_n2" ~ "hurdle_gamma_log",
      variable == "dcca_axis_1" ~ "hurdle_gamma_log",
      variable %in% c(
        "n1_divided_by_n0",
        "n2_divided_by_n1",
        "density_diversity",
        "density_turnover"
      ) ~ "zero_one_inflated_beta_logit",
      analysis == "pap_temporal" ~ "student_identity",
      .default = "student_identity"
    )

  return(res_family_key)
}
