#' @title Pair strict-radius H1 results on identical analytical units
#' @description
#' Pivot 250 km and 500 km source values onto one row per analytical unit,
#' calculate 500-minus-250 deltas, and classify ranking and estimability changes.
#' @param data_source Long radius-specific result table.
#' @param key_cols Columns defining one analytical unit.
#' @param status_col Model-status column.
#' @param balance_cols Numeric source columns for paired differences.
#' @param ranking_cols Ranking columns; the first defines robustness classes.
#' @param estimable_statuses Status values treated as estimable.
#' @return One row per analytical unit with full source values and deltas.
#' @examples
#' \dontrun{
#' prepare_spd_radius_paired_comparison(
#'   results,
#'   key_cols = "dataset_id",
#'   balance_cols = "signed_difference",
#'   ranking_cols = "signed_ranking",
#'   estimable_statuses = "estimated"
#' )
#' }
prepare_spd_radius_paired_comparison <- function(
  data_source,
  key_cols,
  status_col = "status",
  balance_cols,
  ranking_cols,
  estimable_statuses
) {
  required_columns <-
    unique(c(
      key_cols,
      "radius_km",
      status_col,
      balance_cols,
      ranking_cols
    ))
  assertthat::assert_that(
    is.data.frame(data_source),
    is.character(key_cols),
    length(key_cols) > 0L,
    is.character(balance_cols),
    length(balance_cols) > 0L,
    is.character(ranking_cols),
    length(ranking_cols) > 0L,
    is.character(estimable_statuses),
    all(required_columns %in% names(data_source)),
    setequal(unique(data_source[["radius_km"]]), c(250L, 500L)),
    msg = "SPD radius pairing inputs do not satisfy the contract."
  )

  data_keys <-
    data_source |>
    dplyr::count(
      dplyr::across(
        dplyr::all_of(c(key_cols, "radius_km"))
      ),
      name = "n"
    )

  if (any(data_keys[["n"]] != 1L)) {
    cli::cli_abort(
      "SPD radius pairing requires unique analytical-unit radius keys."
    )
  }

  data_wide <-
    data_source |>
    dplyr::select(dplyr::all_of(required_columns)) |>
    tidyr::pivot_wider(
      names_from = "radius_km",
      values_from = dplyr::all_of(
        c(status_col, balance_cols, ranking_cols)
      ),
      names_glue = "{.value}_{radius_km}_km"
    )

  for (balance_col in balance_cols) {
    column_250 <- stringr::str_c(balance_col, "_250_km")
    column_500 <- stringr::str_c(balance_col, "_500_km")
    delta_column <- stringr::str_c(balance_col, "_delta_500_minus_250")
    data_wide[[delta_column]] <-
      data_wide[[column_500]] - data_wide[[column_250]]
  }

  status_250 <- stringr::str_c(status_col, "_250_km")
  status_500 <- stringr::str_c(status_col, "_500_km")
  ranking_primary <- ranking_cols[[1]]
  primary_reversal <-
    stringr::str_c(ranking_primary, "_reversal")

  data_wide <-
    data_wide |>
    dplyr::mutate(
      profile_250_km_present = !is.na(.data[[status_250]]),
      profile_500_km_present = !is.na(.data[[status_500]]),
      estimable_250_km = .data[[status_250]] %in% estimable_statuses,
      estimable_500_km = .data[[status_500]] %in% estimable_statuses,
      matched_estimable =
        .data[["estimable_250_km"]] &
          .data[["estimable_500_km"]],
      status_changed = dplyr::coalesce(
        .data[[status_250]] != .data[[status_500]],
        TRUE
      )
    )

  for (ranking_col in ranking_cols) {
    ranking_250 <- stringr::str_c(ranking_col, "_250_km")
    ranking_500 <- stringr::str_c(ranking_col, "_500_km")
    reversal_col <- stringr::str_c(ranking_col, "_reversal")
    data_wide[[reversal_col]] <-
      data_wide[["matched_estimable"]] &
      !is.na(data_wide[[ranking_250]]) &
      !is.na(data_wide[[ranking_500]]) &
      data_wide[[ranking_250]] != data_wide[[ranking_500]]
  }

  data_wide <-
    data_wide |>
    dplyr::mutate(
      ranking_reversal = .data[[primary_reversal]],
      robustness_classification = dplyr::case_when(
        !.data[["profile_250_km_present"]] ~
          "missing_250_km_result",
        !.data[["profile_500_km_present"]] ~
          "missing_500_km_result",
        .data[["estimable_250_km"]] &
          !.data[["estimable_500_km"]] ~ "lost_estimability",
        !.data[["estimable_250_km"]] &
          .data[["estimable_500_km"]] ~ "gained_estimability",
        !.data[["matched_estimable"]] ~ "not_estimable_at_either_radius",
        .data[["ranking_reversal"]] ~ "ranking_reversal",
        .default = "same_ranking"
      ),
      material_change = .data[["robustness_classification"]] %in%
        c(
          "lost_estimability",
          "gained_estimability",
          "ranking_reversal"
        )
    )

  return(data_wide)
}
