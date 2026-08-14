#' @title Summarise adjusted spatial importance at one level
#' @description
#' Calculate a weighted mean of record-level adjusted balances for one
#' aggregation level and profile.
#' @param data_adjusted Records containing `adjusted_balance`.
#' @param weight_col Weight column name.
#' @param group_vars Character grouping columns.
#' @param level_name Aggregation-level label.
#' @param profile_name Profile label.
#' @return A weighted adjusted-balance summary tibble.
#' @examples
#' \dontrun{
#' summarise_spatial_importance_level(
#'   data_adjusted = adjusted_records,
#'   weight_col = "signed_weight",
#'   group_vars = character(),
#'   level_name = "overall",
#'   profile_name = "signed"
#' )
#' }
summarise_spatial_importance_level <- function(
  data_adjusted,
  weight_col,
  group_vars,
  level_name,
  profile_name
) {
  assertthat::assert_that(
    is.data.frame(data_adjusted),
    assertthat::is.string(weight_col),
    is.character(group_vars),
    assertthat::is.string(level_name),
    assertthat::is.string(profile_name),
    all(c("adjusted_balance", weight_col, group_vars) %in%
      names(data_adjusted)),
    msg = "Adjusted importance inputs do not satisfy the contract."
  )

  res_summary <-
    data_adjusted |>
    dplyr::filter(
      is.finite(.data[["adjusted_balance"]]),
      is.finite(.data[[weight_col]]),
      .data[[weight_col]] > 0
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      adjusted_balance = stats::weighted.mean(
        .data[["adjusted_balance"]],
        .data[[weight_col]]
      ),
      n_records = dplyr::n(),
      weight_sum = sum(.data[[weight_col]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      aggregation_level = level_name,
      profile = profile_name,
      .before = 1L
    )

  return(res_summary)
}
