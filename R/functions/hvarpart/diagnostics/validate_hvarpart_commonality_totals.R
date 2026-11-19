#' @title Validate that commonality fractions sum to the model total
#' @description
#' Check, for every fitted model, that the commonality fractions sum to the
#' `Total` fraction and to the total adjusted R-squared reported in the
#' component table, and abort when any model differs by more than the
#' tolerance.
#' @param data_commonality
#' Output of `summarise_hvarpart_commonality_fractions()`.
#' @param data_components
#' Component table with the `id_columns`, `model_profile`, and
#' `total_adjusted_r_squared`, such as the `components` element of
#' `summarise_local_temporal_hvarpart_models()`.
#' @param id_columns
#' Character vector of columns identifying one fitted model.
#' @param model_profile
#' Component profile whose total is compared.
#' @param tolerance
#' Non-negative number. Maximum allowed absolute difference.
#' @return
#' A tibble with one row per fitted model: the `id_columns`, `sum_fractions`,
#' `total_fraction`, `total_adjusted_r_squared`, `difference_total`,
#' `difference_component`, and `passed`.
#' @details
#' rdacca.hp rounds fractions to four decimals and the total explained
#' variation to three, so the default tolerance is `1e-3`. A fitted model
#' with commonality fractions but no component total is a failure.
#' @examples
#' \dontrun{
#' validate_hvarpart_commonality_totals(commonality, components)
#' }
validate_hvarpart_commonality_totals <- function(
  data_commonality,
  data_components,
  id_columns = c("model_id", "dataset_id"),
  model_profile = "human_climate_time",
  tolerance = 1e-3
) {
  assertthat::assert_that(
    is.data.frame(data_commonality),
    is.data.frame(data_components),
    is.character(id_columns),
    length(id_columns) > 0L,
    all(c(id_columns, "fraction", "total_fraction") %in%
      names(data_commonality)),
    all(c(id_columns, "model_profile", "total_adjusted_r_squared") %in%
      names(data_components)),
    assertthat::is.string(model_profile),
    assertthat::is.number(tolerance),
    tolerance >= 0,
    msg = "Commonality total check inputs do not satisfy the contract."
  )

  data_sums <-
    data_commonality |>
    dplyr::summarise(
      sum_fractions = sum(.data[["fraction"]]),
      total_fraction = dplyr::first(.data[["total_fraction"]]),
      .by = dplyr::all_of(id_columns)
    )

  data_totals <-
    data_components |>
    dplyr::filter(.data[["model_profile"]] == .env[["model_profile"]]) |>
    dplyr::distinct(
      dplyr::across(dplyr::all_of(c(id_columns, "total_adjusted_r_squared")))
    )

  assertthat::assert_that(
    !anyDuplicated(data_totals[id_columns]),
    msg = "Each fitted model needs exactly one component total."
  )

  res <-
    data_sums |>
    dplyr::left_join(data_totals, by = id_columns) |>
    dplyr::mutate(
      difference_total =
        .data[["sum_fractions"]] - .data[["total_fraction"]],
      difference_component =
        .data[["sum_fractions"]] - .data[["total_adjusted_r_squared"]],
      passed = is.finite(.data[["difference_component"]]) &
        abs(.data[["difference_total"]]) <= tolerance &
        abs(.data[["difference_component"]]) <= tolerance
    )

  if (
    any(!res[["passed"]])
  ) {
    n_failed <- sum(!res[["passed"]])

    cli::cli_abort(c(
      "HVarPart commonality fractions do not sum to the model total.",
      "x" = "{n_failed} fitted model{?s} failed the check."
    ))
  }

  return(res)
}
