#' @title Evaluate one brms model
#' @description
#' Compute LOO and Rhat lifecycle diagnostics for a fitted `brms` model.
#' @param mod Fitted `brmsfit` object or `NA_real_`.
#' @param pareto_k_threshold Numeric Pareto-k threshold.
#' @param loo_threshold Numeric maximum proportion above Pareto-k threshold.
#' @param rhat_threshold Numeric Rhat threshold.
#' @param rhat_threshold_quantile Numeric Rhat quantile to compare.
#' @return Tibble with pass/fail diagnostic fields.
#' @examples
#' \dontrun{
#' diagnostics <- evaluate_brms_model(mod)
#' }
evaluate_brms_model <- function(
  mod,
  pareto_k_threshold = 0.7,
  loo_threshold = 0.1,
  rhat_threshold = 1.1,
  rhat_threshold_quantile = 0.9
) {
  if (
    is.atomic(mod) && all(is.na(mod))
  ) {
    res_diagnostics <-
      tibble::tibble(
        last_run_rhat_test_pass = FALSE,
        last_run_rhat_test_value = NA_real_,
        last_run_loo_test_pass = FALSE,
        last_run_loo_test_value = NA_real_,
        need_to_run = TRUE
      )

    return(res_diagnostics)
  }

  assertthat::assert_that(
    inherits(mod, "brmsfit"),
    msg = "`mod` must be a fitted brms model or `NA_real_`."
  )

  loo_res <-
    brms::loo(mod)

  loo_length <-
    length(loo_res[["diagnostics"]][["pareto_k"]])

  loo_value <-
    sum(loo_res[["diagnostics"]][["pareto_k"]] >= pareto_k_threshold) /
    loo_length

  pass_loo_test <-
    loo_value <= loo_threshold

  rhat_res <-
    brms::rhat(mod)

  avg_rhat_value <-
    mean(rhat_res, na.rm = TRUE)

  pass_rhat_test <-
    as.logical(
      stats::quantile(
        rhat_res,
        probs = rhat_threshold_quantile,
        na.rm = TRUE
      ) <= rhat_threshold
    )

  res_diagnostics <-
    tibble::tibble(
      last_run_rhat_test_pass = pass_rhat_test,
      last_run_rhat_test_value = avg_rhat_value,
      last_run_loo_test_pass = pass_loo_test,
      last_run_loo_test_value = loo_value,
      need_to_run = isFALSE(pass_rhat_test) | isFALSE(pass_loo_test)
    )

  return(res_diagnostics)
}
