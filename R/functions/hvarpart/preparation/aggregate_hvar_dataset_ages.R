#' @title Collapse HVarPart inputs to one row per dataset and age
#' @description
#' Verify that predictor values are invariant within dataset-age combinations and
#' average repeated response estimates before HVarPart analyses.
#' @param data_source Data frame with identifier and nested data columns.
#' @param response_vars Response columns to average within age.
#' @param predictor_vars Predictor columns required to be invariant within age.
#' @param id_col Core identifier column.
#' @param data_col Nested data column.
#' @param age_col Age column in each nested data frame.
#' @return A list containing collapsed nested data and an audit table.
#' @examples
#' \dontrun{
#' aggregate_hvar_dataset_ages(
#'   data_source = nested_hvar_data,
#'   response_vars = c("n0", "n1"),
#'   predictor_vars = c("spd", "temp_annual")
#' )
#' }
aggregate_hvar_dataset_ages <- function(
  data_source,
  response_vars,
  predictor_vars,
  id_col = "dataset_id",
  data_col = "data_merge",
  age_col = "age"
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c(id_col, data_col) %in% names(data_source)),
    is.list(data_source[[data_col]]),
    is.character(response_vars),
    length(response_vars) > 0L,
    is.character(predictor_vars),
    length(predictor_vars) > 0L,
    !anyNA(data_source[[id_col]]),
    !anyDuplicated(data_source[[id_col]]),
    msg = "Core-age collapse inputs do not satisfy the contract."
  )

  required_columns <-
    unique(c(age_col, response_vars, predictor_vars))
  invalid_inputs <-
    data_source[[data_col]] |>
    purrr::map_lgl(
      .f = ~ !is.data.frame(.x) ||
        !all(required_columns %in% names(.x))
    )

  if (
    any(invalid_inputs)
  ) {
    cli::cli_abort(
      "Every nested dataset must contain age, response, and predictor columns."
    )
  }

  invalid_ages <-
    data_source[[data_col]] |>
    purrr::map_lgl(
      .f = ~ !is.numeric(.x[[age_col]]) ||
        any(!is.finite(.x[[age_col]]))
    )

  if (
    any(invalid_ages)
  ) {
    cli::cli_abort(
      "Every dataset-age unit must have a finite numeric age."
    )
  }

  list_collapsed <-
    data_source[[data_col]] |>
    purrr::map(
      .f = ~ {
        data_conflicts <-
          .x |>
          dplyr::select(
            dplyr::all_of(c(age_col, predictor_vars))
          ) |>
          tidyr::pivot_longer(
            cols = -dplyr::all_of(age_col),
            names_to = "predictor",
            values_to = "value"
          ) |>
          dplyr::filter(!is.na(.data[["value"]])) |>
          dplyr::group_by(
            .data[[age_col]],
            .data[["predictor"]]
          ) |>
          dplyr::summarise(
            n_values = dplyr::n_distinct(.data[["value"]]),
            .groups = "drop"
          ) |>
          dplyr::filter(.data[["n_values"]] > 1L)

        if (
          nrow(data_conflicts) > 0L
        ) {
          cli::cli_abort(
            "Predictor values conflict within at least one dataset-age unit."
          )
        }

        data_collapsed <-
          .x |>
          dplyr::select(dplyr::all_of(required_columns)) |>
          dplyr::group_by(.data[[age_col]]) |>
          dplyr::summarise(
            dplyr::across(
              dplyr::all_of(response_vars),
              ~ {
                if (
                  all(is.na(.x))
                ) {
                  return(NA_real_)
                }

                res <-
                  mean(.x, na.rm = TRUE)

                return(res)
              }
            ),
            dplyr::across(
              dplyr::all_of(predictor_vars),
              ~ dplyr::first(
                .x[!is.na(.x)],
                default = NA_real_
              )
            ),
            .groups = "drop"
          ) |>
          dplyr::arrange(.data[[age_col]])

        return(data_collapsed)
      }
    )

  data_audit <-
    tibble::tibble(
      !!id_col := data_source[[id_col]],
      n_input_rows = purrr::map_int(data_source[[data_col]], nrow),
      n_unique_ages = purrr::map_int(list_collapsed, nrow)
    ) |>
    dplyr::mutate(
      n_collapsed_rows =
        .data[["n_input_rows"]] - .data[["n_unique_ages"]],
      had_repeated_ages = .data[["n_collapsed_rows"]] > 0L
    )
  data_collapsed <-
    data_source |>
    dplyr::select(-dplyr::all_of(data_col)) |>
    dplyr::mutate(!!data_col := list_collapsed)

  res <-
    list(
      data = data_collapsed,
      audit = data_audit
    )

  return(res)
}
