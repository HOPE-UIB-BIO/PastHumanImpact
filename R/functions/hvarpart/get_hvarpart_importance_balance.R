#' @title Calculate human-climate importance balance
#' @description
#' Converts paired zero-truncated human and climate allocations into a bounded
#' balance equal to human allocation minus climate allocation.
#' @param data_summary Predictor-level pooled HVarPart allocations.
#' @param group_vars Character vector naming the balance grouping columns.
#' @return
#' A tibble with one row per group, separate human and climate allocations,
#' model counts when available, and `importance_balance`.
#' @details
#' A balance of one indicates exclusively human importance, zero indicates
#' equal importance, and minus one indicates exclusively climate importance.
#' @examples
#' \dontrun{
#' get_hvarpart_importance_balance(
#'   data_summary = temporal_summary,
#'   group_vars = c("analysis", "region", "age")
#' )
#' }
get_hvarpart_importance_balance <- function(
  data_summary,
  group_vars
) {
  required <- c(
    group_vars,
    "predictor",
    "pooled_allocation"
  )
  assertthat::assert_that(
    is.data.frame(data_summary),
    is.character(group_vars),
    length(group_vars) > 0L,
    !anyNA(group_vars),
    !anyDuplicated(group_vars),
    all(required %in% names(data_summary)),
    msg = "Importance balance inputs do not satisfy the required contract."
  )

  duplicate_rows <-
    data_summary |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(c(group_vars, "predictor"))
      ),
      name = "n_rows"
    ) |>
    dplyr::filter(.data[["n_rows"]] != 1L)

  assertthat::assert_that(
    nrow(duplicate_rows) == 0L,
    msg = "Each balance group and predictor must occur exactly once."
  )

  retained_columns <- c(
    group_vars,
    "predictor",
    "pooled_allocation"
  )
  if (
    "n_models" %in% names(data_summary)
  ) {
    retained_columns <- c(retained_columns, "n_models")
  }

  data_balance <-
    data_summary |>
    dplyr::filter(
      .data[["predictor"]] %in% c("human", "climate")
    ) |>
    dplyr::select(dplyr::all_of(retained_columns)) |>
    tidyr::pivot_wider(
      names_from = "predictor",
      values_from = "pooled_allocation"
    )

  if (
    !all(c("human", "climate") %in% names(data_balance)) ||
      any(!is.finite(data_balance[["human"]])) ||
      any(!is.finite(data_balance[["climate"]]))
  ) {
    cli::cli_abort(
      "Every balance group must contain finite human and climate allocations."
    )
  }

  data_balance <-
    data_balance |>
    dplyr::mutate(
      importance_balance =
        .data[["human"]] - .data[["climate"]]
    )

  if (
    any(abs(data_balance[["importance_balance"]]) > 1 + 1e-10)
  ) {
    cli::cli_abort(
      "Human-climate importance balances must be between -1 and 1."
    )
  }

  return(data_balance)
}
