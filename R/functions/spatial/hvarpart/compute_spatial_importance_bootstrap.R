#' @title Bootstrap the overall dbMEM-adjusted human-climate balance
#' @description
#' Compute a percentile bootstrap interval for the overall dbMEM-adjusted
#' balance of a fitted spatial importance aggregation, resampling records
#' within region-by-climate-zone strata and refitting the weighted
#' aggregation model with the dbMEMs selected in the observed fit.
#' @param data_records
#' The records passed to `fit_spatial_importance()`, in the same order, with
#' columns `region`, `climatezone`, and the response and weight columns of
#' `profile`.
#' @param result_spatial_importance
#' Output of `fit_spatial_importance()` for `data_records`.
#' @param profile
#' `"zero_truncated"` (response `zero_balance`, weight `zero_weight`) or
#' `"signed"` (response `signed_difference`, weight `signed_weight`).
#' @param n_bootstrap
#' Number of bootstrap replicates.
#' @param seed
#' Integer random seed; the global random state is left unchanged.
#' @return
#' A one-row tibble with columns `profile`, `adjusted_balance` (the observed
#' overall estimate), `lwr_95` and `upr_95` (2.5th and 97.5th percentiles of
#' the replicates), `n_records`, `n_bootstrap`, and `n_failed_replicates`
#' (replicates without a finite estimate).
#' @details
#' The aggregation model is the weighted linear model of
#' `fit_spatial_importance_profile()`: one intercept per region-by-climate-zone
#' stratum plus the selected dbMEMs. The overall estimate is the weighted mean
#' of the predictions with every dbMEM set to zero. Each replicate draws, with
#' replacement, as many records from each stratum as it holds, keeping each
#' record's dbMEM values, so the interval is conditional on the dbMEM
#' selection. The function stops with an error if the observed estimate
#' differs from the overall estimate stored in `result_spatial_importance`.
#' @examples
#' \dontrun{
#' res_fit <- fit_spatial_importance(records)
#' compute_spatial_importance_bootstrap(records, res_fit, "zero_truncated")
#' }
compute_spatial_importance_bootstrap <- function(
  data_records,
  result_spatial_importance,
  profile = c("zero_truncated", "signed"),
  n_bootstrap = 1999L,
  seed = 1234L
) {
  profile <- match.arg(profile)

  vec_profile_columns <-
    switch(
      profile,
      zero_truncated = c(response = "zero_balance", weight = "zero_weight"),
      signed = c(response = "signed_difference", weight = "signed_weight")
    )

  assertthat::assert_that(
    is.data.frame(data_records),
    all(c("region", "climatezone", vec_profile_columns) %in%
      names(data_records)),
    is.list(result_spatial_importance),
    all(c("dbmem", "selection", "estimates") %in%
      names(result_spatial_importance)),
    is.numeric(n_bootstrap),
    length(n_bootstrap) == 1L,
    n_bootstrap >= 1,
    is.numeric(seed),
    length(seed) == 1L,
    msg = "Spatial bootstrap inputs do not satisfy the contract."
  )

  data_basis <-
    result_spatial_importance[["dbmem"]][["basis"]]

  vec_selected <-
    result_spatial_importance[["selection"]][["selected_names"]]

  assertthat::assert_that(
    nrow(data_basis) == nrow(data_records),
    all(vec_selected %in% names(data_basis)),
    msg = "Spatial bootstrap inputs do not satisfy the contract."
  )

  vec_stratum <-
    interaction(
      data_records[["region"]],
      data_records[["climatezone"]],
      drop = TRUE,
      lex.order = TRUE
    )

  mat_strata <-
    stats::model.matrix(~ 0 + vec_stratum)

  mat_mem <-
    as.matrix(data_basis[vec_selected])

  mat_design <-
    cbind(mat_strata, mat_mem)

  # Predictions with every dbMEM set to zero use only stratum columns.
  mat_design_adjusted <-
    cbind(mat_strata, mat_mem * 0)

  vec_response <-
    data_records[[vec_profile_columns[["response"]]]]

  vec_weight <-
    data_records[[vec_profile_columns[["weight"]]]]

  value_observed <-
    compute_adjusted_balance_estimate(
      mat_design = mat_design,
      mat_design_adjusted = mat_design_adjusted,
      vec_response = vec_response,
      vec_weight = vec_weight
    )

  value_stored <-
    result_spatial_importance[["estimates"]] |>
    dplyr::filter(
      .data[["aggregation_level"]] == "overall",
      .data[["profile"]] == .env[["profile"]]
    ) |>
    dplyr::pull("adjusted_balance")

  if (
    length(value_stored) != 1L ||
      !isTRUE(abs(value_stored - value_observed) < 1e-8)
  ) {
    cli::cli_abort(
      c(
        "The refitted aggregation does not reproduce the stored estimate.",
        "x" = "Stored {value_stored}; refitted {value_observed}."
      )
    )
  }

  list_strata_rows <-
    split(seq_len(nrow(data_records)), vec_stratum)

  vec_replicates <-
    withr::with_seed(
      seed = seed,
      code = purrr::map_dbl(
        seq_len(n_bootstrap),
        .f = ~ {
          vec_rows <-
            list_strata_rows |>
            purrr::map(
              .f = ~ .x[sample.int(length(.x), length(.x), replace = TRUE)]
            ) |>
            unlist(use.names = FALSE)

          compute_adjusted_balance_estimate(
            mat_design = mat_design[vec_rows, , drop = FALSE],
            mat_design_adjusted = mat_design_adjusted[vec_rows, , drop = FALSE],
            vec_response = vec_response[vec_rows],
            vec_weight = vec_weight[vec_rows]
          )
        }
      )
    )

  vec_finite <-
    vec_replicates[is.finite(vec_replicates)]

  vec_interval <-
    if (
      length(vec_finite) == 0L
    ) {
      c(NA_real_, NA_real_)
    } else {
      stats::quantile(vec_finite, probs = c(0.025, 0.975), names = FALSE)
    }

  res <-
    tibble::tibble(
      profile = profile,
      adjusted_balance = value_observed,
      lwr_95 = vec_interval[1],
      upr_95 = vec_interval[2],
      n_records = nrow(data_records),
      n_bootstrap = as.integer(n_bootstrap),
      n_failed_replicates = as.integer(sum(!is.finite(vec_replicates)))
    )

  return(res)
}
