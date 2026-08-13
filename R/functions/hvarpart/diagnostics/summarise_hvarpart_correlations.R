#' @title Summarise adjusted R-squared and human importance correlations
#' @description
#' Calculate Spearman and Pearson correlations only where at least three
#' observations and non-zero variation are available.
#' @param data_values Model-level output from
#' `compute_hvarpart_correlation_values()`.
#' @param group_vars Optional character vector of grouping columns.
#' @param importance_column Human importance column to correlate.
#' @return A tibble with sample size, variation flags, and correlation values.
summarise_hvarpart_correlations <- function(
  data_values,
  group_vars = character(),
  importance_column = c(
    "human_importance_bounded",
    "human_importance_signed"
  )
) {
  importance_column <-
    match.arg(importance_column)

  required_columns <-
    c(
      "adjusted_r_squared",
      importance_column
    )

  assertthat::assert_that(
    is.data.frame(data_values),
    all(c(required_columns, group_vars) %in% names(data_values)),
    msg = "`data_values` does not satisfy the correlation summary contract."
  )
  assertthat::assert_that(
    is.character(group_vars),
    !anyNA(group_vars),
    !anyDuplicated(group_vars),
    msg = "`group_vars` must contain unique column names."
  )

  data_complete <-
    data_values |>
    dplyr::filter(
      is.finite(.data[["adjusted_r_squared"]]),
      is.finite(.data[[importance_column]])
    ) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      n_models = dplyr::n(),
      has_x_variation =
        dplyr::n_distinct(.data[["adjusted_r_squared"]]) > 1L,
      has_y_variation =
        dplyr::n_distinct(.data[[importance_column]]) > 1L,
      correlation_available =
        .data[["n_models"]] >= 3L &
        .data[["has_x_variation"]] &
        .data[["has_y_variation"]],
      spearman_rho =
        if (
          .data[["correlation_available"]]
        ) {
          stats::cor(
            .data[["adjusted_r_squared"]],
            .data[[importance_column]],
            method = "spearman"
          )
        } else {
          NA_real_
        },
      pearson_r =
        if (
          .data[["correlation_available"]]
        ) {
          stats::cor(
            .data[["adjusted_r_squared"]],
            .data[[importance_column]],
            method = "pearson"
          )
        } else {
          NA_real_
        },
      importance_profile = importance_column,
      .groups = "drop"
    )

  return(data_complete)
}
