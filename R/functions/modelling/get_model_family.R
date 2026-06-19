#' @title Get brms model family from a stable key
#' @description
#' Convert a character family key into a family object for model fitting. This
#' keeps model specs as data and avoids evaluating family strings.
#' @param family_key Character scalar naming the family.
#' @return A family object accepted by `brms::brm()`.
#' @details
#' Supported keys are `gaussian_identity`, `student_identity`,
#' `bernoulli_logit`, and `hurdle_gamma_log`.
#' @examples
#' \dontrun{
#' family_object <- get_model_family("student_identity")
#' }
get_model_family <- function(family_key) {
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
      hurdle_gamma_log = brms::hurdle_gamma(link = "log"),
      NULL
    )

  assertthat::assert_that(
    !is.null(res_family),
    msg = paste0("Unknown `family_key`: ", family_key, ".")
  )

  return(res_family)
}
