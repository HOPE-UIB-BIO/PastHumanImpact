#' @title Summarise paired SPD-radius comparisons
#' @description
#' Count matched units, ranking reversals, and material changes and describe the
#' continuous 500-minus-250 balance difference without a threshold.
#' @param data_comparison Paired comparison table.
#' @param delta_col Continuous delta column to summarise.
#' @param group_cols Optional grouping columns.
#' @param summary_level Label for the output aggregation.
#' @return Summary table with counts, proportions, median, and interquartile
#'   range.
#' @examples
#' \dontrun{
#' summarise_spd_radius_paired_results(
#'   comparison,
#'   "signed_difference_delta_500_minus_250"
#' )
#' }
summarise_spd_radius_paired_results <- function(
  data_comparison,
  delta_col,
  group_cols = character(),
  summary_level = "overall"
) {
  required_columns <-
    c(
      "estimable_250_km",
      "estimable_500_km",
      "profile_250_km_present",
      "profile_500_km_present",
      "matched_estimable",
      "status_changed",
      "ranking_reversal",
      "material_change",
      "robustness_classification",
      delta_col,
      group_cols
    )
  assertthat::assert_that(
    is.data.frame(data_comparison),
    assertthat::is.string(delta_col),
    is.character(group_cols),
    assertthat::is.string(summary_level),
    all(required_columns %in% names(data_comparison)),
    msg = "SPD radius paired-summary inputs do not satisfy the contract."
  )

  data_grouped <-
    if (length(group_cols) == 0L) {
      dplyr::group_by(data_comparison)
    } else {
      dplyr::group_by(
        data_comparison,
        dplyr::across(dplyr::all_of(group_cols))
      )
    }

  res_summary <-
    data_grouped |>
    dplyr::summarise(
      n_units = dplyr::n(),
      n_profile_250_km_present =
        sum(.data[["profile_250_km_present"]]),
      n_profile_500_km_present =
        sum(.data[["profile_500_km_present"]]),
      n_estimable_250_km = sum(.data[["estimable_250_km"]]),
      n_estimable_500_km = sum(.data[["estimable_500_km"]]),
      n_matched_estimable = sum(.data[["matched_estimable"]]),
      n_status_changes = sum(.data[["status_changed"]]),
      n_ranking_reversals = sum(.data[["ranking_reversal"]]),
      n_lost_estimability = sum(
        .data[["robustness_classification"]] ==
          "lost_estimability"
      ),
      n_gained_estimability = sum(
        .data[["robustness_classification"]] ==
          "gained_estimability"
      ),
      n_material_changes = sum(.data[["material_change"]]),
      proportion_ranking_reversals = dplyr::if_else(
        .data[["n_matched_estimable"]] > 0L,
        .data[["n_ranking_reversals"]] /
        .data[["n_matched_estimable"]],
        NA_real_
      ),
      proportion_material_changes = dplyr::if_else(
        .data[["n_units"]] > 0L,
        .data[["n_material_changes"]] / .data[["n_units"]],
        NA_real_
      ),
      median_delta = stats::median(
        .data[[delta_col]][.data[["matched_estimable"]]],
        na.rm = TRUE
      ),
      delta_q25 = stats::quantile(
        .data[[delta_col]][.data[["matched_estimable"]]],
        probs = 0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      delta_q75 = stats::quantile(
        .data[[delta_col]][.data[["matched_estimable"]]],
        probs = 0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      .groups = "drop"
    ) |>
    dplyr::mutate(summary_level = summary_level, .before = 1L)

  return(res_summary)
}
