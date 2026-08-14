#' @title Resolve brms model family from a stable key
#' @description
#' Convert a character family key into a family object for model fitting. This
#' keeps model specs as data and avoids evaluating family strings.
#' @param family_key Character scalar naming the family.
#' @return A family object accepted by `brms::brm()`.
#' @details
#' Supported keys are `gaussian_identity`, `student_identity`,
#' `bernoulli_logit`, `gamma_log`, `hurdle_gamma_log`, and
#' `zero_one_inflated_beta_logit`.
#' @examples
#' \dontrun{
#' family_object <- resolve_model_family("student_identity")
#' }
resolve_model_family <- function(family_key) {
  assertthat::assert_that(
    is.character(family_key),
    length(family_key) == 1,
    !is.na(family_key),
    msg = "`family_key` must be a single character value."
  )

  assertthat::assert_that(
    requireNamespace("brms", quietly = TRUE),
    msg = "`brms` must be installed to build model families."
  )

  res_family <-
    switch(
      family_key,
      gaussian_identity = stats::gaussian(link = "identity"),
      student_identity = brms::student(link = "identity"),
      bernoulli_logit = brms::bernoulli(link = "logit"),
      gamma_log = brms::brmsfamily("Gamma", link = "log"),
      hurdle_gamma_log = brms::hurdle_gamma(link = "log"),
      zero_one_inflated_beta_logit =
        brms::brmsfamily("zero_one_inflated_beta", link = "logit"),
      NULL
    )

  assertthat::assert_that(
    !is.null(res_family),
    msg = paste0("Unknown `family_key`: ", family_key, ".")
  )

  return(res_family)
}
