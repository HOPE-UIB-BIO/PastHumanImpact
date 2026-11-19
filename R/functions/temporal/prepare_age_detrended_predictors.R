#' @title Prepare age-detrended predictors within nested series
#' @description
#' Add, for each requested predictor, a column holding the residuals of a
#' linear regression of that predictor on age, fitted separately within each
#' nested analytical unit.
#' @param data_source
#' A data frame with one row per analytical unit and a list column of
#' per-unit data frames.
#' @param predictor_columns
#' Character vector of numeric predictor columns to detrend.
#' @param age_column
#' Name of the numeric age column inside each nested data frame.
#' @param data_col
#' Name of the list column holding the nested data frames.
#' @param suffix
#' Suffix appended to each predictor name to form the output column.
#' @param tolerance
#' Non-negative number. Residuals whose standard deviation is at most
#' `tolerance` times the predictor's standard deviation are set to zero.
#' @return
#' `data_source` with each nested data frame extended by the columns
#' `<predictor><suffix>`. Rows, row order, and existing columns are unchanged.
#' @details
#' Residuals come from `y ~ 1 + age` fitted on the rows where both the
#' predictor and age are finite; other rows receive `NA`. Within a unit the
#' residuals have mean zero and zero correlation with age. A constant
#' predictor, or one that is an exact linear function of age, carries no
#' variation beyond the linear age trend, so its residuals are set exactly to
#' zero; downstream fitters then drop the column as constant instead of
#' scaling floating-point noise. When age is constant within a unit the
#' residuals are deviations from the predictor mean.
#' @examples
#' \dontrun{
#' prepare_age_detrended_predictors(
#'   data_source = data_within,
#'   predictor_columns = c("spd_sqrt", "kk10_fraction")
#' )
#' }
prepare_age_detrended_predictors <- function(
  data_source,
  predictor_columns,
  age_column = "age",
  data_col = "data_merge",
  suffix = "_age_detrended",
  tolerance = sqrt(.Machine$double.eps)
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    assertthat::is.string(data_col),
    data_col %in% names(data_source),
    is.list(data_source[[data_col]]),
    is.character(predictor_columns),
    length(predictor_columns) > 0L,
    !anyDuplicated(predictor_columns),
    assertthat::is.string(age_column),
    assertthat::is.string(suffix),
    nchar(suffix) > 0L,
    assertthat::is.number(tolerance),
    tolerance >= 0,
    msg = "Age-detrending inputs do not satisfy the contract."
  )

  vec_output_columns <-
    stringr::str_c(predictor_columns, suffix)

  flag_units_valid <-
    data_source[[data_col]] |>
    purrr::map_lgl(
      .f = ~ is.data.frame(.x) &&
        all(c(age_column, predictor_columns) %in% names(.x)) &&
        !any(vec_output_columns %in% names(.x)) &&
        is.numeric(.x[[age_column]]) &&
        all(purrr::map_lgl(.x[predictor_columns], is.numeric))
    )

  assertthat::assert_that(
    all(flag_units_valid),
    msg = paste(
      "Every nested data frame needs numeric age and predictor columns",
      "and no existing detrended columns."
    )
  )

  res <- data_source

  res[[data_col]] <-
    data_source[[data_col]] |>
    purrr::map(
      .f = ~ {
        data_unit <- .x

        vec_age <- data_unit[[age_column]]

        list_detrended <-
          predictor_columns |>
          purrr::map(
            .f = ~ {
              vec_values <- data_unit[[.x]]

              flag_complete <- is.finite(vec_values) & is.finite(vec_age)

              vec_residuals <- rep(NA_real_, length(vec_values))

              vec_complete <- vec_values[flag_complete]

              if (
                length(vec_complete) > 0L &&
                  dplyr::n_distinct(vec_complete) <= 1L
              ) {
                vec_residuals[flag_complete] <- 0
              } else if (
                length(vec_complete) > 1L
              ) {
                mat_design <-
                  cbind(intercept = 1, age = vec_age[flag_complete])

                vec_fit_residuals <-
                  stats::lm.fit(x = mat_design, y = vec_complete)[[
                    "residuals"
                  ]]

                flag_no_variation <-
                  stats::sd(vec_fit_residuals) <=
                    tolerance * stats::sd(vec_complete)

                vec_residuals[flag_complete] <-
                  if (
                    isTRUE(flag_no_variation)
                  ) {
                    0
                  } else {
                    unname(vec_fit_residuals)
                  }
              }

              vec_residuals
            }
          ) |>
          rlang::set_names(vec_output_columns)

        dplyr::bind_cols(
          data_unit,
          tibble::as_tibble(list_detrended)
        )
      }
    )

  return(res)
}
