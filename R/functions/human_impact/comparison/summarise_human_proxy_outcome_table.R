#' @title Summarise the human-proxy outcome table
#' @description
#' Classify every decision metric and assemble the outcome table: one outcome
#' class per table row and table column, read on the headline profile, with
#' the other profiles beside it and a flag when the class changes between
#' profiles.
#' @param data_metrics
#' Data frame with one row per decision metric, the columns named in
#' `row_columns` and `table_columns`, and `profile`, `estimate`, `lwr_95`,
#' `upr_95`, and `n_units`. Further columns (for example `proxy_support`)
#' are carried into `long` and `summary`.
#' @param headline_profile
#' Profile used for the outcome class of the table.
#' @param row_columns
#' Character vector of columns identifying a table row.
#' @param table_columns
#' Character vector of columns whose combinations form the table columns.
#' @param group_columns
#' Character vector, a subset of `row_columns`, identifying the groups of
#' rows over which `all_cells_same_class` is evaluated.
#' @return
#' A list with `long` (`data_metrics` with `outcome_class`), `summary` (one
#' row per table cell with the headline row of `data_metrics`,
#' `outcome_class`, the classes of the other profiles as
#' `outcome_class_<profile>`, and `class_changes_between_profiles`), and
#' `table` (one row per combination of `row_columns`, one column of headline
#' classes per combination of `table_columns`, named by joining their values
#' with `"__"`, and `all_cells_same_class`, which is `TRUE` when every cell
#' of the group has the same class).
#' @details
#' A table cell is one combination of `row_columns` and `table_columns`; it
#' must occur at most once per profile. Classes come from
#' `classify_human_climate_outcome()`. A missing class does not count as a
#' change between profiles. The defaults give one row per analysis,
#' comparison, proxy, and support and one column per window.
#' @examples
#' \dontrun{
#' summarise_human_proxy_outcome_table(metrics)
#' }
summarise_human_proxy_outcome_table <- function(
  data_metrics,
  headline_profile = "zero_truncated",
  row_columns = c("analysis", "comparison", "human_proxy", "proxy_support"),
  table_columns = "age_window",
  group_columns = c("analysis", "comparison")
) {
  assertthat::assert_that(
    is.data.frame(data_metrics),
    is.character(row_columns),
    length(row_columns) > 0L,
    is.character(table_columns),
    length(table_columns) > 0L,
    is.character(group_columns),
    all(group_columns %in% row_columns),
    length(intersect(row_columns, table_columns)) == 0L,
    all(c(row_columns, table_columns, "profile", "estimate", "lwr_95",
      "upr_95", "n_units") %in% names(data_metrics)),
    assertthat::is.string(headline_profile),
    headline_profile %in% data_metrics[["profile"]],
    !anyDuplicated(
      data_metrics[c(row_columns, table_columns, "profile")]
    ),
    msg = "Outcome-table inputs do not satisfy the contract."
  )

  vec_cell_columns <-
    c(row_columns, table_columns)

  res_long <-
    data_metrics |>
    dplyr::mutate(
      outcome_class = classify_human_climate_outcome(
        lwr = .data[["lwr_95"]],
        upr = .data[["upr_95"]]
      )
    )

  data_other_classes <-
    res_long |>
    dplyr::filter(.data[["profile"]] != headline_profile) |>
    dplyr::select(
      dplyr::all_of(c(vec_cell_columns, "profile", "outcome_class"))
    ) |>
    tidyr::pivot_wider(
      names_from = "profile",
      values_from = "outcome_class",
      names_prefix = "outcome_class_"
    )

  data_changes <-
    res_long |>
    dplyr::summarise(
      class_changes_between_profiles = dplyr::n_distinct(
        .data[["outcome_class"]],
        na.rm = TRUE
      ) > 1L,
      .by = dplyr::all_of(vec_cell_columns)
    )

  res_summary <-
    res_long |>
    dplyr::filter(.data[["profile"]] == headline_profile) |>
    dplyr::left_join(data_other_classes, by = vec_cell_columns) |>
    dplyr::left_join(data_changes, by = vec_cell_columns)

  res_table <-
    res_summary |>
    dplyr::select(dplyr::all_of(c(vec_cell_columns, "outcome_class"))) |>
    tidyr::pivot_wider(
      id_cols = dplyr::all_of(row_columns),
      names_from = dplyr::all_of(table_columns),
      names_sep = "__",
      values_from = "outcome_class"
    ) |>
    dplyr::left_join(
      res_summary |>
        dplyr::summarise(
          all_cells_same_class =
            dplyr::n_distinct(.data[["outcome_class"]]) == 1L &&
              !anyNA(.data[["outcome_class"]]),
          .by = dplyr::all_of(group_columns)
        ),
      by = group_columns
    )

  res <-
    list(
      long = res_long,
      summary = res_summary,
      table = res_table
    )

  return(res)
}
