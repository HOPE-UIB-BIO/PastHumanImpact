#' @title Validate age-detrended predictors within nested series
#' @description
#' Check that every detrended predictor has mean zero and no linear
#' correlation with age within each nested analytical unit, and abort when
#' any unit violates that contract.
#' @param data_source
#' A data frame with one row per analytical unit and a list column of
#' per-unit data frames.
#' @param detrended_columns
#' Character vector of detrended predictor columns to check.
#' @param age_column
#' Name of the numeric age column inside each nested data frame.
#' @param unit_column
#' Name of the unit identifier column in `data_source`.
#' @param data_col
#' Name of the list column holding the nested data frames.
#' @param tolerance
#' Non-negative number. Maximum allowed absolute correlation with age, and
#' maximum allowed absolute mean relative to `max(1, max(abs(values)))`.
#' @return
#' A tibble with one row per unit and detrended column: the unit identifier,
#' `predictor`, `n_finite`, `constant`, `correlation_with_age`, `mean_value`,
#' and `passed`.
#' @details
#' Only rows where both the detrended value and age are finite are used.
#' Constant columns, including all-zero columns, pass with
#' `correlation_with_age = NA`, as do units in which age is constant.
#' @examples
#' \dontrun{
#' validate_age_detrended_predictors(
#'   data_source = data_within,
#'   detrended_columns = "spd_sqrt_age_detrended"
#' )
#' }
validate_age_detrended_predictors <- function(
  data_source,
  detrended_columns,
  age_column = "age",
  unit_column = "dataset_id",
  data_col = "data_merge",
  tolerance = 1e-8
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    assertthat::is.string(unit_column),
    assertthat::is.string(data_col),
    all(c(unit_column, data_col) %in% names(data_source)),
    is.list(data_source[[data_col]]),
    is.character(detrended_columns),
    length(detrended_columns) > 0L,
    assertthat::is.string(age_column),
    assertthat::is.number(tolerance),
    tolerance >= 0,
    msg = "Detrending check inputs do not satisfy the contract."
  )

  assertthat::assert_that(
    all(purrr::map_lgl(
      data_source[[data_col]],
      .f = ~ is.data.frame(.x) &&
        all(c(age_column, detrended_columns) %in% names(.x))
    )),
    msg = "Every nested data frame needs age and the detrended columns."
  )

  res <-
    seq_len(nrow(data_source)) |>
    purrr::map(
      .f = ~ {
        index_unit <- .x

        data_unit <- data_source[[data_col]][[index_unit]]

        vec_age <- data_unit[[age_column]]

        detrended_columns |>
          purrr::map(
            .f = ~ {
              vec_values <- data_unit[[.x]]

              flag_complete <- is.finite(vec_values) & is.finite(vec_age)

              vec_complete <- vec_values[flag_complete]

              vec_age_complete <- vec_age[flag_complete]

              flag_constant <- dplyr::n_distinct(vec_complete) <= 1L

              flag_age_constant <- dplyr::n_distinct(vec_age_complete) <= 1L

              value_correlation <-
                if (
                  flag_constant || flag_age_constant
                ) {
                  NA_real_
                } else {
                  stats::cor(vec_complete, vec_age_complete)
                }

              value_mean <-
                if (
                  length(vec_complete) == 0L
                ) {
                  NA_real_
                } else {
                  mean(vec_complete)
                }

              value_scale <-
                max(1, abs(vec_complete))

              flag_passed <-
                (is.na(value_correlation) ||
                  abs(value_correlation) <= tolerance) &&
                (is.na(value_mean) ||
                  abs(value_mean) <= tolerance * value_scale)

              tibble::tibble(
                unit = data_source[[unit_column]][[index_unit]],
                predictor = .x,
                n_finite = length(vec_complete),
                constant = flag_constant,
                correlation_with_age = value_correlation,
                mean_value = value_mean,
                passed = flag_passed
              )
            }
          ) |>
          dplyr::bind_rows()
      }
    ) |>
    dplyr::bind_rows() |>
    dplyr::rename(!!unit_column := "unit")

  if (
    any(!res[["passed"]])
  ) {
    data_failed <-
      res |>
      dplyr::filter(!.data[["passed"]])

    cli::cli_abort(c(
      "Detrended predictors still correlate with age or have non-zero mean.",
      "x" = "{nrow(data_failed)} unit-predictor combination{?s} failed."
    ))
  }

  return(res)
}
