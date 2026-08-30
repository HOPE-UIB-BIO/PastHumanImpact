#' @title Reconcile SPD-radius paired summaries
#' @description
#' Independently reconstruct every grouped count and continuous summary from
#' paired source rows and fail when an exported summary disagrees.
#' @param data_comparison Paired analytical-unit source table.
#' @param data_summary Published paired summary table.
#' @param delta_col Continuous delta column used by the summary.
#' @return Row-level reconciliation audit.
#' @examples
#' \dontrun{
#' diagnose_spd_radius_summary_reconciliation(paired, summary, "delta")
#' }
diagnose_spd_radius_summary_reconciliation <- function(
  data_comparison,
  data_summary,
  delta_col
) {
  required_comparison <-
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
      delta_col
    )
  required_summary <-
    c(
      "summary_level",
      "n_units",
      "n_profile_250_km_present",
      "n_profile_500_km_present",
      "n_estimable_250_km",
      "n_estimable_500_km",
      "n_matched_estimable",
      "n_status_changes",
      "n_ranking_reversals",
      "n_lost_estimability",
      "n_gained_estimability",
      "n_material_changes",
      "proportion_ranking_reversals",
      "proportion_material_changes",
      "median_delta",
      "delta_q25",
      "delta_q75"
    )
  assertthat::assert_that(
    is.data.frame(data_comparison),
    is.data.frame(data_summary),
    assertthat::is.string(delta_col),
    all(required_comparison %in% names(data_comparison)),
    all(required_summary %in% names(data_summary)),
    msg = "SPD radius reconciliation inputs do not satisfy the contract."
  )

  candidate_group_columns <-
    intersect(
      c("region", "climatezone", "age"),
      names(data_summary)
    )
  list_audit <-
    seq_len(nrow(data_summary)) |>
    purrr::map(
      ~ {
        summary_row <- data_summary[.x, , drop = FALSE]
        data_subset <- data_comparison

        for (group_col in candidate_group_columns) {
          group_value <- summary_row[[group_col]][[1]]
          if (!is.na(group_value)) {
            data_subset <-
              data_subset |>
              dplyr::filter(.data[[group_col]] == group_value)
          }
        }

        matched_delta <-
          data_subset[[delta_col]][data_subset[["matched_estimable"]]]
        expected <-
          tibble::tibble(
            n_units = nrow(data_subset),
            n_profile_250_km_present =
              sum(data_subset[["profile_250_km_present"]]),
            n_profile_500_km_present =
              sum(data_subset[["profile_500_km_present"]]),
            n_estimable_250_km =
              sum(data_subset[["estimable_250_km"]]),
            n_estimable_500_km =
              sum(data_subset[["estimable_500_km"]]),
            n_matched_estimable =
              sum(data_subset[["matched_estimable"]]),
            n_status_changes =
              sum(data_subset[["status_changed"]]),
            n_ranking_reversals =
              sum(data_subset[["ranking_reversal"]]),
            n_lost_estimability = sum(
              data_subset[["robustness_classification"]] ==
                "lost_estimability"
            ),
            n_gained_estimability = sum(
              data_subset[["robustness_classification"]] ==
                "gained_estimability"
            ),
            n_material_changes =
              sum(data_subset[["material_change"]]),
            proportion_ranking_reversals = if (
              n_matched_estimable > 0L
            ) {
              n_ranking_reversals / n_matched_estimable
            } else {
              NA_real_
            },
            proportion_material_changes = if (n_units > 0L) {
              n_material_changes / n_units
            } else {
              NA_real_
            },
            median_delta = stats::median(matched_delta, na.rm = TRUE),
            delta_q25 = stats::quantile(
              matched_delta,
              probs = 0.25,
              na.rm = TRUE,
              names = FALSE
            ),
            delta_q75 = stats::quantile(
              matched_delta,
              probs = 0.75,
              na.rm = TRUE,
              names = FALSE
            )
          )
        value_columns <- names(expected)
        differences <-
          purrr::map_lgl(
            value_columns,
            ~ isTRUE(all.equal(
              summary_row[[.x]][[1]],
              expected[[.x]][[1]],
              tolerance = 1e-12,
              check.attributes = FALSE
            ))
          )

        summary_row |>
          dplyr::select(
            dplyr::any_of(
              c("summary_level", candidate_group_columns)
            )
          ) |>
          dplyr::mutate(
            all_values_reconciled = all(differences),
            checked_columns = stringr::str_c(
              value_columns,
              collapse = ";"
            )
          )
      }
    )
  res_audit <- dplyr::bind_rows(list_audit)

  if (!all(res_audit[["all_values_reconciled"]])) {
    cli::cli_abort(
      "At least one SPD radius summary disagrees with its paired source rows."
    )
  }

  return(res_audit)
}
