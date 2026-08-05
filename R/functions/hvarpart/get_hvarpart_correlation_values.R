#' @title Get model values for HVarPart fit-importance correlations
#' @description
#' Return one row per eligible model with adjusted R-squared and both the
#' zero-truncated bounded and full-range signed human importance allocations.
#' @param data_importance Canonical predictor-level HVarPart importance data.
#' @param id_cols Character vector of model identifier columns to retain.
#' @return A tibble with one row per eligible model.
get_hvarpart_correlation_values <- function(
  data_importance,
  id_cols
) {
  required_columns <-
    c(
      "model_id",
      "predictor",
      "individual",
      "total_adjusted_r_squared",
      "is_importance_eligible"
    )

  assertthat::assert_that(
    is.data.frame(data_importance),
    all(c(required_columns, id_cols) %in% names(data_importance)),
    msg = "`data_importance` does not satisfy the correlation contract."
  )
  assertthat::assert_that(
    is.character(id_cols),
    length(id_cols) > 0L,
    !anyNA(id_cols),
    !anyDuplicated(id_cols),
    msg = "`id_cols` must name one or more identifier columns."
  )

  data_values <-
    data_importance |>
    dplyr::filter(.data[["is_importance_eligible"]]) |>
    dplyr::group_by(
      dplyr::across(dplyr::all_of(id_cols))
    ) |>
    dplyr::mutate(
      truncated_individual = pmax(.data[["individual"]], 0),
      truncated_total = sum(.data[["truncated_individual"]])
    ) |>
    dplyr::filter(.data[["predictor"]] == "human") |>
    dplyr::ungroup() |>
    dplyr::transmute(
      dplyr::across(dplyr::all_of(id_cols)),
      adjusted_r_squared = .data[["total_adjusted_r_squared"]],
      human_importance_bounded =
        .data[["truncated_individual"]] / .data[["truncated_total"]],
      human_importance_signed =
        .data[["individual"]] / .data[["total_adjusted_r_squared"]]
    )

  invalid_values <-
    data_values |>
    dplyr::filter(
      !is.finite(.data[["adjusted_r_squared"]]) |
        !is.finite(.data[["human_importance_bounded"]]) |
        !is.finite(.data[["human_importance_signed"]])
    )

  assertthat::assert_that(
    nrow(invalid_values) == 0L,
    msg = "Eligible models must yield finite correlation values."
  )

  return(data_values)
}
