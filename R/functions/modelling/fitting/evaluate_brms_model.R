#' @title Evaluate one brms model
#' @description
#' Compute LOO and sampler diagnostics for a fitted `brms` model. LOO results
#' are recorded for model assessment, while reruns are requested only when
#' sampler diagnostics fail.
#' @param mod Fitted `brmsfit` object or `NA_real_`.
#' @param pareto_k_threshold Numeric Pareto-k threshold.
#' @param loo_threshold Numeric maximum proportion above Pareto-k threshold.
#' @param rhat_threshold Numeric Rhat threshold.
#' @param rhat_threshold_quantile Numeric Rhat quantile to compare.
#' @param max_treedepth_threshold Integer tree-depth threshold used to count
#' transitions that reached the configured maximum.
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
  rhat_threshold_quantile = 0.9,
  max_treedepth_threshold = 10
) {
  if (
    is.atomic(mod) && all(is.na(mod))
  ) {
    res_diagnostics <-
      tibble::tibble(
        last_run_rhat_test_pass = FALSE,
        last_run_rhat_test_value = NA_real_,
        last_run_rhat_q90 = NA_real_,
        last_run_rhat_max = NA_real_,
        last_run_neff_ratio_min = NA_real_,
        last_run_divergent_transitions = NA_integer_,
        last_run_max_treedepth_transitions = NA_integer_,
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
  assertthat::assert_that(
    assertthat::is.count(max_treedepth_threshold),
    msg = "`max_treedepth_threshold` must be a positive integer."
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

  q90_rhat_value <-
    as.numeric(
      stats::quantile(
        rhat_res,
        probs = rhat_threshold_quantile,
        na.rm = TRUE
      )
    )

  max_rhat_value <-
    max(rhat_res, na.rm = TRUE)

  pass_rhat_test <-
    as.logical(
      q90_rhat_value <= rhat_threshold
    )

  neff_ratio_res <-
    brms::neff_ratio(mod)

  min_neff_ratio_value <-
    min(neff_ratio_res, na.rm = TRUE)

  nuts_params_res <-
    brms::nuts_params(mod)

  divergent_transitions <-
    sum(
      nuts_params_res[["Parameter"]] == "divergent__" &
        nuts_params_res[["Value"]] == 1
    )

  max_treedepth_transitions <-
    sum(
      nuts_params_res[["Parameter"]] == "treedepth__" &
        nuts_params_res[["Value"]] >= max_treedepth_threshold
    )

  res_diagnostics <-
    tibble::tibble(
      last_run_rhat_test_pass = pass_rhat_test,
      last_run_rhat_test_value = avg_rhat_value,
      last_run_rhat_q90 = q90_rhat_value,
      last_run_rhat_max = max_rhat_value,
      last_run_neff_ratio_min = min_neff_ratio_value,
      last_run_divergent_transitions = divergent_transitions,
      last_run_max_treedepth_transitions = max_treedepth_transitions,
      last_run_loo_test_pass = pass_loo_test,
      last_run_loo_test_value = loo_value,
      need_to_run = isFALSE(pass_rhat_test) |
        divergent_transitions > 0 |
        max_treedepth_transitions > 0
    )

  return(res_diagnostics)
}
