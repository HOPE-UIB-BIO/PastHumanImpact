#' @title Summarise spatial HVarPart balance
#' @description
#' Calculate pooled zero-truncated human-minus-climate balances for requested
#' grouping variables.
#' @param data_importance Canonical predictor-level HVarPart importance data.
#' @param group_vars Character vector of grouping columns.
#' @param region_levels Ordered region labels.
#' @return A tibble containing pooled allocations and importance balances.
#' @examples
#' \dontrun{
#' summarise_spatial_hvarpart_balance(
#'   data_importance = importance,
#'   group_vars = c("analysis", "region"),
#'   region_levels = c("Europe", "Asia")
#' )
#' }
summarise_spatial_hvarpart_balance <- function(
  data_importance,
  group_vars,
  region_levels
) {
  assertthat::assert_that(
    is.data.frame(data_importance),
    is.character(group_vars),
    length(group_vars) > 0L,
    all(group_vars %in% names(data_importance)),
    is.character(region_levels),
    "region" %in% group_vars,
    msg = "Spatial HVarPart summary inputs do not satisfy the contract."
  )

  data_summary <-
    summarise_hvarpart_importance(
      data_importance = data_importance |>
        dplyr::filter(.data[["analysis"]] == "spatial_spd"),
      group_vars = group_vars,
      profile = "zero_truncated"
    ) |>
    dplyr::select(
      dplyr::all_of(
        c(
          group_vars,
          "predictor",
          "pooled_allocation",
          "n_models"
        )
      )
    ) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "pooled_allocation"
    )
  if (
    !all(c("human", "climate") %in% names(data_summary))
  ) {
    cli::cli_abort(
      "Pooled summaries must contain human and climate allocations."
    )
  }
  data_summary <-
    data_summary |>
    dplyr::mutate(
      importance_balance = .data[["human"]] - .data[["climate"]],
      region = factor(.data[["region"]], levels = region_levels)
    ) |>
    dplyr::arrange(.data[["region"]])

  return(data_summary)
}
