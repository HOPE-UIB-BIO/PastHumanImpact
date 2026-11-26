#' @title Fit the spatial H1 decision metric of one human-proxy run
#' @description
#' Aggregate record-level human-climate balances with dbMEM spatial control
#' and return the overall adjusted balance with a 95 % bootstrap interval for
#' the zero-truncated, signed, and exclude-negative profiles.
#' @param data_records
#' Estimable record balances (see `filter_estimable_balance_records()`) with
#' the columns required by `fit_spatial_importance()` and the individual
#' contributions `human`, `climate`, and `time`.
#' @param n_bootstrap
#' Number of bootstrap replicates per profile.
#' @param seed
#' Integer random seed for the dbMEM selection and the bootstraps.
#' @param ...
#' Further arguments passed to `fit_spatial_importance()`.
#' @return
#' A list with `metric`, a tibble with one row per profile (`signed`,
#' `zero_truncated`, `exclude_negative`) and columns `profile`,
#' `adjusted_balance`, `lwr_95`, `upr_95`, `n_records`, `n_human_dominant`,
#' `n_bootstrap`, `n_failed_replicates`, `fit_status`, and `error_message`;
#' `estimates`, the overall, regional, and stratum estimates of every
#' profile; and `moran_diagnostics`.
#' @details
#' The zero-truncated and signed profiles come from one call of
#' `fit_spatial_importance()` on all records. The exclude-negative profile
#' keeps only records whose human, climate, and time contributions are all
#' non-negative, refits the aggregation (including the dbMEM selection), and
#' reports its zero-truncated balance, which on these records equals
#' (human - climate) / (human + climate). Intervals come from
#' `compute_spatial_importance_bootstrap()`. `n_human_dominant` counts
#' records with a positive balance of the profile. A failed exclude-negative
#' fit is returned with `fit_status = "fit_error"` and its message.
#' @examples
#' \dontrun{
#' fit_human_proxy_spatial_decision_metric(records, permutations = 999L)
#' }
fit_human_proxy_spatial_decision_metric <- function(
  data_records,
  n_bootstrap = 1999L,
  seed = 1234L,
  ...
) {
  vec_required <-
    c(
      "model_id", "long", "lat", "region", "climatezone",
      "signed_difference", "signed_weight", "zero_balance", "zero_weight",
      "human", "climate", "time"
    )

  assertthat::assert_that(
    is.data.frame(data_records),
    all(vec_required %in% names(data_records)),
    nrow(data_records) > 0L,
    !anyDuplicated(data_records[["model_id"]]),
    all(is.finite(data_records[["signed_difference"]])),
    all(data_records[["signed_weight"]] > 0),
    all(is.finite(data_records[["zero_balance"]])),
    all(data_records[["zero_weight"]] > 0),
    msg = "Spatial decision-metric records do not satisfy the contract."
  )

  list_extra <-
    rlang::list2(...)

  res_all <-
    rlang::exec(
      fit_spatial_importance,
      data_records = data_records,
      seed = as.integer(seed),
      !!!list_extra
    )

  data_metric_all <-
    c("zero_truncated", "signed") |>
    purrr::map(
      .f = ~ compute_spatial_importance_bootstrap(
        data_records = data_records,
        result_spatial_importance = res_all,
        profile = .x,
        n_bootstrap = n_bootstrap,
        seed = seed
      )
    ) |>
    dplyr::bind_rows() |>
    dplyr::mutate(
      n_human_dominant = c(
        sum(data_records[["zero_balance"]] > 0),
        sum(data_records[["signed_difference"]] > 0)
      ),
      fit_status = "estimated",
      error_message = NA_character_
    )

  data_non_negative <-
    data_records |>
    dplyr::filter(
      .data[["human"]] >= 0,
      .data[["climate"]] >= 0,
      .data[["time"]] >= 0
    )

  fit_non_negative <-
    purrr::safely(
      .f = ~ {
        res_fit <-
          rlang::exec(
            fit_spatial_importance,
            data_records = data_non_negative,
            seed = as.integer(seed),
            !!!list_extra
          )

        data_boot <-
          compute_spatial_importance_bootstrap(
            data_records = data_non_negative,
            result_spatial_importance = res_fit,
            profile = "zero_truncated",
            n_bootstrap = n_bootstrap,
            seed = seed
          )

        list(fit = res_fit, bootstrap = data_boot)
      }
    )()

  data_metric_non_negative <-
    if (
      is.null(fit_non_negative[["error"]])
    ) {
      fit_non_negative[["result"]][["bootstrap"]] |>
        dplyr::mutate(
          profile = "exclude_negative",
          n_human_dominant = sum(data_non_negative[["zero_balance"]] > 0),
          fit_status = "estimated",
          error_message = NA_character_
        )
    } else {
      tibble::tibble(
        profile = "exclude_negative",
        adjusted_balance = NA_real_,
        lwr_95 = NA_real_,
        upr_95 = NA_real_,
        n_records = nrow(data_non_negative),
        n_bootstrap = as.integer(n_bootstrap),
        n_failed_replicates = NA_integer_,
        n_human_dominant = sum(data_non_negative[["zero_balance"]] > 0),
        fit_status = "fit_error",
        error_message = conditionMessage(fit_non_negative[["error"]])
      )
    }

  data_estimates_non_negative <-
    if (
      is.null(fit_non_negative[["error"]])
    ) {
      fit_non_negative[["result"]][["fit"]][["estimates"]] |>
        dplyr::filter(.data[["profile"]] == "zero_truncated") |>
        dplyr::mutate(profile = "exclude_negative")
    } else {
      tibble::tibble()
    }

  data_moran_non_negative <-
    if (
      is.null(fit_non_negative[["error"]])
    ) {
      fit_non_negative[["result"]][["fit"]][["moran_diagnostics"]] |>
        dplyr::filter(.data[["profile"]] == "zero_truncated") |>
        dplyr::mutate(profile = "exclude_negative")
    } else {
      tibble::tibble()
    }

  res_metric <-
    dplyr::bind_rows(data_metric_all, data_metric_non_negative) |>
    dplyr::mutate(n_human_dominant = as.integer(.data[["n_human_dominant"]])) |>
    dplyr::select(
      dplyr::all_of(c(
        "profile", "adjusted_balance", "lwr_95", "upr_95", "n_records",
        "n_human_dominant", "n_bootstrap", "n_failed_replicates",
        "fit_status", "error_message"
      ))
    )

  res <-
    list(
      metric = res_metric,
      estimates = dplyr::bind_rows(
        res_all[["estimates"]],
        data_estimates_non_negative
      ),
      moran_diagnostics = dplyr::bind_rows(
        res_all[["moran_diagnostics"]],
        data_moran_non_negative
      )
    )

  return(res)
}
