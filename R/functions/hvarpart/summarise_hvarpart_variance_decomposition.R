#' @title Summarise HVarPart variance decompositions
#' @description
#' Summarise signed and bounded variance components while retaining model
#' counts and model-level equal-weight means.
#' @param data_decomposition Model-level output from
#' `get_hvarpart_variance_decomposition()`.
#' @param group_vars Character vector of grouping columns.
#' @return A tibble with one row per group and explicit availability counts.
summarise_hvarpart_variance_decomposition <- function(
  data_decomposition,
  group_vars
) {
  component_columns <-
    c(
      "total_adjusted_r_squared",
      "unique_human",
      "unique_climate",
      "shared",
      "unexplained",
      "bounded_total_adjusted_r_squared",
      "bounded_unique_human",
      "bounded_unique_climate",
      "bounded_shared",
      "bounded_unexplained"
    )

  required_columns <-
    c(
      "model_id",
      "has_finite_decomposition",
      "accounting_within_tolerance",
      component_columns
    )

  assertthat::assert_that(
    is.data.frame(data_decomposition),
    all(c(required_columns, group_vars) %in% names(data_decomposition)),
    msg = "`data_decomposition` does not satisfy the summary contract."
  )
  assertthat::assert_that(
    is.character(group_vars),
    length(group_vars) > 0L,
    !anyNA(group_vars),
    !anyDuplicated(group_vars),
    msg = "`group_vars` must name one or more grouping columns."
  )

  data_summary <-
    data_decomposition |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(group_vars))
    ) |>
    dplyr::summarise(
      n_models = dplyr::n_distinct(.data[["model_id"]]),
      n_available = sum(.data[["has_finite_decomposition"]]),
      n_accounting_valid = sum(.data[["accounting_within_tolerance"]]),
      dplyr::across(
        dplyr::all_of(component_columns),
        list(
          sum = ~ sum(.x[.data[["has_finite_decomposition"]]]),
          mean = ~ mean(.x[.data[["has_finite_decomposition"]]])
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  return(data_summary)
}
