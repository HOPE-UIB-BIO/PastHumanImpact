#' @title Summarise the temporal H1 decision metric of one human-proxy run
#' @description
#' Convert estimable region-by-age HVarPart compositions into unit-level
#' human, climate, and space contributions and report the pooled
#' human-minus-climate allocation with a 95 % block-bootstrap interval for
#' the zero-truncated, signed, and exclude-negative profiles.
#' @param data_composition
#' Estimable compositions in long form (`composition_common` of
#' `prepare_human_proxy_comparison_tables()`), with columns `region`, `age`,
#' `predictor` (`human`, `climate`, `space`), and `individual`.
#' @param block_length
#' Number of consecutive ages per bootstrap block within a region.
#' @param n_bootstrap
#' Number of bootstrap replicates.
#' @param seed
#' Integer random seed.
#' @return
#' A list with `metric`, a tibble with one row per profile and columns
#' `profile`, `pooled_difference`, `lwr_95`, `upr_95`, `mean_difference`,
#' `n_units`, `n_human_greater`, `n_bootstrap`, and `n_failed_replicates`,
#' and `units`, the unit-level contributions (`region`, `age`, `human`,
#' `climate`, `space`).
#' @details
#' Estimates come from `compute_pooled_human_climate_allocation()` and
#' intervals from `compute_temporal_allocation_bootstrap()`. The function
#' stops with an error when a unit lacks one of the three predictors or has
#' one twice.
#' @examples
#' \dontrun{
#' summarise_human_proxy_temporal_decision_metric(compositions)
#' }
summarise_human_proxy_temporal_decision_metric <- function(
  data_composition,
  block_length = 3L,
  n_bootstrap = 1999L,
  seed = 1234L
) {
  assertthat::assert_that(
    is.data.frame(data_composition),
    all(c("region", "age", "predictor", "individual") %in%
      names(data_composition)),
    msg = "Temporal decision-metric inputs do not satisfy the contract."
  )

  vec_profiles <-
    c("zero_truncated", "signed", "exclude_negative")

  data_counts <-
    data_composition |>
    dplyr::count(.data[["region"]], .data[["age"]], .data[["predictor"]])

  data_unit_checks <-
    data_counts |>
    dplyr::summarise(
      is_complete = setequal(
        .data[["predictor"]],
        c("human", "climate", "space")
      ) &&
        all(.data[["n"]] == 1L),
      .by = c("region", "age")
    )

  if (
    !all(data_unit_checks[["is_complete"]])
  ) {
    cli::cli_abort(
      "Every unit needs exactly one human, climate, and space contribution."
    )
  }

  res_units <-
    data_composition |>
    dplyr::select(
      dplyr::all_of(c("region", "age", "predictor", "individual"))
    ) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "individual"
    ) |>
    dplyr::select(
      dplyr::all_of(c("region", "age", "human", "climate", "space"))
    ) |>
    dplyr::arrange(.data[["region"]], .data[["age"]])

  data_estimates <-
    compute_pooled_human_climate_allocation(
      data_units = res_units,
      profile = vec_profiles
    )

  data_intervals <-
    if (
      nrow(res_units) == 0L
    ) {
      tibble::tibble(
        profile = vec_profiles,
        lwr_95 = NA_real_,
        upr_95 = NA_real_,
        n_bootstrap = as.integer(n_bootstrap),
        n_failed_replicates = NA_integer_
      )
    } else {
      compute_temporal_allocation_bootstrap(
        data_units = res_units,
        profile = vec_profiles,
        block_length = block_length,
        n_bootstrap = n_bootstrap,
        seed = seed
      )
    }

  res_metric <-
    data_estimates |>
    dplyr::left_join(data_intervals, by = "profile") |>
    dplyr::select(
      dplyr::all_of(c(
        "profile", "pooled_difference", "lwr_95", "upr_95",
        "mean_difference", "n_units", "n_human_greater", "n_bootstrap",
        "n_failed_replicates"
      ))
    )

  res <-
    list(
      metric = res_metric,
      units = res_units
    )

  return(res)
}
