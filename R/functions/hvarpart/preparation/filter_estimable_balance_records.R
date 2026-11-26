#' @title Filter estimable record-level human-climate balances
#' @description
#' Keep the time-controlled record balances that come from an estimated model
#' and have finite balances with positive weights, as used by the spatial
#' aggregation.
#' @param data_records
#' Output of `prepare_time_controlled_importance_records()`, with columns
#' `status`, `signed_difference`, `signed_weight`, `zero_balance`, and
#' `zero_weight`.
#' @param estimated_statuses
#' Character vector of model statuses that count as estimated.
#' @return
#' `data_records` restricted to estimable rows, with unchanged columns.
#' @details
#' A row is estimable when its status is in `estimated_statuses`, its
#' `signed_difference` and `zero_balance` are finite, and its `signed_weight`
#' and `zero_weight` are positive. This is the filter of
#' `prepare_human_proxy_comparison_tables()`.
#' @examples
#' \dontrun{
#' filter_estimable_balance_records(records)
#' }
filter_estimable_balance_records <- function(
  data_records,
  estimated_statuses = c(
    "estimated",
    "estimated_residual_temporal_dependence"
  )
) {
  assertthat::assert_that(
    is.data.frame(data_records),
    all(c(
      "status", "signed_difference", "signed_weight", "zero_balance",
      "zero_weight"
    ) %in% names(data_records)),
    is.character(estimated_statuses),
    length(estimated_statuses) > 0L,
    msg = "Balance records do not satisfy the estimable-record contract."
  )

  res <-
    data_records |>
    dplyr::filter(
      .data[["status"]] %in% estimated_statuses,
      is.finite(.data[["signed_difference"]]),
      .data[["signed_weight"]] > 0,
      is.finite(.data[["zero_balance"]]),
      .data[["zero_weight"]] > 0
    )

  return(res)
}
