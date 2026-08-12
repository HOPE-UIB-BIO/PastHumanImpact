#' @title Summarise one spatial-importance profile
#' @description
#' Calculate weighted importance balances for one response profile and one
#' aggregation level under a named sensitivity scenario.
#' @param data_subset Core-level spatial importance records.
#' @param profile_name Profile label.
#' @param balance_col Balance column name.
#' @param weight_col Weight column name.
#' @param group_vars Character grouping columns.
#' @param level_name Aggregation-level label.
#' @param sensitivity_type Sensitivity scenario label.
#' @param omitted_group Optional omitted-group label.
#' @param distance_value Optional thinning distance.
#' @param repetition_value Optional thinning repetition.
#' @return A weighted importance summary tibble.
#' @examples
#' \dontrun{
#' summarise_spatial_importance_profile(
#'   data_subset = records,
#'   profile_name = "signed",
#'   balance_col = "signed_balance",
#'   weight_col = "signed_weight",
#'   group_vars = character(),
#'   level_name = "overall",
#'   sensitivity_type = "baseline"
#' )
#' }
summarise_spatial_importance_profile <- function(
  data_subset,
  profile_name,
  balance_col,
  weight_col,
  group_vars,
  level_name,
  sensitivity_type,
  omitted_group = NA_character_,
  distance_value = NA_real_,
  repetition_value = NA_integer_
) {
  assertthat::assert_that(
    is.data.frame(data_subset),
    assertthat::is.string(profile_name),
    assertthat::is.string(balance_col),
    assertthat::is.string(weight_col),
    is.character(group_vars),
    assertthat::is.string(level_name),
    assertthat::is.string(sensitivity_type),
    all(c(
      balance_col,
      weight_col,
      group_vars,
      "region",
      "climatezone"
    ) %in% names(data_subset)),
    msg = "Spatial importance profile inputs do not satisfy the contract."
  )

  res_summary <-
    data_subset |>
    dplyr::filter(
      is.finite(.data[[balance_col]]),
      is.finite(.data[[weight_col]]),
      .data[[weight_col]] > 0
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      importance_balance = stats::weighted.mean(
        .data[[balance_col]],
        .data[[weight_col]]
      ),
      n_records = dplyr::n(),
      n_strata = dplyr::n_distinct(
        interaction(.data[["region"]], .data[["climatezone"]])
      ),
      weight_sum = sum(.data[[weight_col]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      sensitivity_type = sensitivity_type,
      omitted_group = omitted_group,
      distance_km = distance_value,
      repetition = repetition_value,
      aggregation_level = level_name,
      profile = profile_name,
      .before = 1L
    )

  return(res_summary)
}
