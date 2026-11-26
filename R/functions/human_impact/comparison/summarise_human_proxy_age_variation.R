#' @title Summarise variation of human proxies among records and through time
#' @description
#' For each proxy, support, and age, summarise how much values vary among
#' records and how much they change from the next older age within records.
#' Used to judge whether a proxy carries information at an age or is
#' near-constant.
#' @param data_values
#' Data frame in the `data_human_proxy_values` contract.
#' @param value_threshold
#' Non-negative numeric scalar. `prop_above_threshold` is the share of
#' records whose value exceeds it. Default `0.01` (one per cent of the area
#' for land-use fractions).
#' @param change_tolerance
#' Non-negative numeric scalar. A record counts as changed when the absolute
#' change from the next older age exceeds it. Default `1e-6`.
#' @return
#' Tibble with one row per `proxy`, `support`, and `age_bp`, and the columns
#' `n_datasets`, `mean`, `sd`, `median`, `lwr_90`, `upr_90`, `max`,
#' `prop_positive`, `prop_above_threshold`, `n_with_change`,
#' `mean_abs_change`, and `prop_changed`. Statistics use finite values only.
#' `lwr_90` and `upr_90` are the 5th and 95th percentiles. The change
#' columns compare each record with its value at the next older age of the
#' same record, proxy, and support; pairs in which either value is missing
#' are skipped. `n_with_change` counts the compared pairs; where it is zero
#' (for example at the oldest age) `mean_abs_change` and `prop_changed` are
#' `NA`.
#' @examples
#' \dontrun{
#' summarise_human_proxy_age_variation(data_values)
#' }
summarise_human_proxy_age_variation <- function(
  data_values,
  value_threshold = 0.01,
  change_tolerance = 1e-6
) {
  assertthat::assert_that(
    is.data.frame(data_values),
    all(
      c("dataset_id", "age_bp", "proxy", "support", "value") %in%
        names(data_values)
    ),
    assertthat::is.number(value_threshold),
    value_threshold >= 0,
    assertthat::is.number(change_tolerance),
    change_tolerance >= 0,
    msg = "Human-proxy age-variation inputs do not satisfy the contract."
  )

  data_changes <-
    data_values |>
    dplyr::group_by(
      .data[["proxy"]],
      .data[["support"]],
      .data[["dataset_id"]]
    ) |>
    dplyr::arrange(.data[["age_bp"]], .by_group = TRUE) |>
    dplyr::mutate(
      value_older = dplyr::lead(.data[["value"]]),
      abs_change = abs(.data[["value"]] - .data[["value_older"]])
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      abs_change = dplyr::if_else(
        is.finite(.data[["abs_change"]]),
        .data[["abs_change"]],
        NA_real_
      )
    )

  data_level_summary <-
    data_changes |>
    dplyr::filter(is.finite(.data[["value"]])) |>
    dplyr::group_by(
      .data[["proxy"]],
      .data[["support"]],
      .data[["age_bp"]]
    ) |>
    dplyr::summarise(
      n_datasets = dplyr::n(),
      mean = mean(.data[["value"]]),
      sd = stats::sd(.data[["value"]]),
      median = stats::median(.data[["value"]]),
      lwr_90 = stats::quantile(.data[["value"]], probs = 0.05, names = FALSE),
      upr_90 = stats::quantile(.data[["value"]], probs = 0.95, names = FALSE),
      max = max(.data[["value"]]),
      prop_positive = mean(.data[["value"]] > 0),
      prop_above_threshold = mean(.data[["value"]] > value_threshold),
      .groups = "drop"
    )

  data_change_summary <-
    data_changes |>
    dplyr::group_by(
      .data[["proxy"]],
      .data[["support"]],
      .data[["age_bp"]]
    ) |>
    dplyr::summarise(
      n_with_change = sum(!is.na(.data[["abs_change"]])),
      mean_abs_change = dplyr::if_else(
        .data[["n_with_change"]] > 0L,
        mean(.data[["abs_change"]], na.rm = TRUE),
        NA_real_
      ),
      prop_changed = dplyr::if_else(
        .data[["n_with_change"]] > 0L,
        mean(.data[["abs_change"]] > change_tolerance, na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    )

  res_variation <-
    data_level_summary |>
    dplyr::left_join(
      data_change_summary,
      by = dplyr::join_by("proxy", "support", "age_bp"),
      relationship = "one-to-one"
    ) |>
    dplyr::mutate(
      n_with_change = dplyr::coalesce(.data[["n_with_change"]], 0L)
    ) |>
    dplyr::arrange(
      .data[["proxy"]],
      .data[["support"]],
      dplyr::desc(.data[["age_bp"]])
    )

  return(res_variation)
}
