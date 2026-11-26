#' @title Prepare HVarPart designs with a shared climate selection
#' @description
#' Combine each analytical unit with every human-proxy specification, reusing
#' the climate block selected once for that unit and filtering only the human
#' block, so models of one unit differ only in the human predictors.
#' @param data_units
#' Data frame with the unit identifier columns and a list column of unit
#' data.
#' @param data_climate_selection
#' Output of `select_shared_climate_predictors()` for the same units.
#' @param model_specifications
#' Data frame with columns `model_id`, `model_label`, `human_candidates`
#' (list), `human_preference` (list), and `exploratory`.
#' @param unit_columns
#' Character vector of columns identifying an analytical unit.
#' @param data_col
#' Name of the list column holding each unit's data frame.
#' @param drop_rows_without_human
#' Logical. If `TRUE`, rows whose human candidates are all missing are removed
#' from a unit before the human block is filtered (records without the proxy
#' in a region-by-age unit). If `FALSE` (default), rows are kept.
#' @param max_cor
#' Maximum absolute within-group correlation for the human block.
#' @param max_vif
#' Maximum within-group variance inflation factor for the human block.
#' @return
#' A tibble with one row per unit and specification: the unit columns,
#' `model_id`, `model_label`, `exploratory`, `selection_status`,
#' `selection_error`, `n_rows_without_human`, `climate_set`, `data_merge`,
#' `predictor_vars` (list with `human` and `climate`), and `selection_audit`.
#' The columns used by `fit_local_temporal_hvarpart_models()` and
#' `fit_local_spatial_hvarpart_models()` match
#' `prepare_local_hvarpart_model_designs()`.
#' @details
#' The climate block is copied unchanged from `data_climate_selection`. The
#' human block is filtered with `select_local_hvarpart_predictor_group()`.
#' `selection_status` is `"human_selection_error"`,
#' `"climate_selection_error"`, `"missing_human_predictor"`,
#' `"missing_climate_predictor"`, or `"eligible_for_design_check"`, in that
#' order of precedence. The function stops with an error when a unit has no
#' climate selection or more than one.
#' @examples
#' \dontrun{
#' prepare_shared_climate_hvarpart_designs(
#'   units, climate, specs, c("window_id", "dataset_id")
#' )
#' }
prepare_shared_climate_hvarpart_designs <- function(
  data_units,
  data_climate_selection,
  model_specifications,
  unit_columns,
  data_col = "data_merge",
  drop_rows_without_human = FALSE,
  max_cor = 0.8,
  max_vif = 5
) {
  assertthat::assert_that(
    is.data.frame(data_units),
    is.character(unit_columns),
    length(unit_columns) > 0L,
    all(c(unit_columns, data_col) %in% names(data_units)),
    is.list(data_units[[data_col]]),
    !anyDuplicated(data_units[unit_columns]),
    is.data.frame(data_climate_selection),
    all(c(unit_columns, "climate_status", "climate_predictors",
      "climate_set", "selection_error", "selection_audit") %in%
      names(data_climate_selection)),
    is.data.frame(model_specifications),
    all(c("model_id", "model_label", "human_candidates", "human_preference",
      "exploratory") %in% names(model_specifications)),
    !anyDuplicated(model_specifications[["model_id"]]),
    is.logical(drop_rows_without_human),
    length(drop_rows_without_human) == 1L,
    !is.na(drop_rows_without_human),
    msg = "Shared-climate design inputs do not satisfy the contract."
  )

  data_climate <-
    data_climate_selection |>
    dplyr::select(
      dplyr::all_of(c(
        unit_columns, "climate_status", "climate_predictors", "climate_set",
        "selection_error", "selection_audit"
      ))
    ) |>
    dplyr::rename(
      climate_error = dplyr::all_of("selection_error"),
      climate_audit = dplyr::all_of("selection_audit")
    )

  data_joined <-
    data_units |>
    dplyr::select(dplyr::all_of(c(unit_columns, data_col))) |>
    dplyr::left_join(
      data_climate |>
        dplyr::mutate(.has_climate = TRUE),
      by = unit_columns,
      relationship = "one-to-one"
    )

  if (
    !all(data_joined[[".has_climate"]] %in% TRUE)
  ) {
    cli::cli_abort(
      "Every analytical unit needs exactly one climate selection."
    )
  }

  res <-
    seq_len(nrow(data_joined)) |>
    purrr::map(
      .f = ~ {
        index_unit <- .x

        data_unit_row <-
          data_joined[index_unit, ]

        data_unit <-
          data_unit_row[[data_col]][[1]]

        vec_climate <-
          data_unit_row[["climate_predictors"]][[1]]

        seq_len(nrow(model_specifications)) |>
          purrr::map(
            .f = ~ {
              index_spec <- .x

              vec_human_candidates <-
                model_specifications[["human_candidates"]][[index_spec]]

              vec_human_preference <-
                model_specifications[["human_preference"]][[index_spec]]

              vec_without_human <-
                data_unit |>
                dplyr::select(dplyr::all_of(vec_human_candidates)) |>
                is.na() |>
                rowSums() |>
                magrittr::equals(length(vec_human_candidates))

              data_unit_model <-
                if (
                  isTRUE(drop_rows_without_human)
                ) {
                  data_unit[!vec_without_human, , drop = FALSE]
                } else {
                  data_unit
                }

              n_without_human <-
                if (
                  isTRUE(drop_rows_without_human)
                ) {
                  sum(vec_without_human)
                } else {
                  0L
                }

              human <-
                select_local_hvarpart_predictor_group(
                  data_source = data_unit_model,
                  candidates = vec_human_candidates,
                  preference = vec_human_preference,
                  group_name = "human",
                  max_cor = max_cor,
                  max_vif = max_vif
                )

              selection_status <-
                dplyr::case_when(
                  human[["status"]] == "selection_error" ~
                    "human_selection_error",
                  data_unit_row[["climate_status"]] == "selection_error" ~
                    "climate_selection_error",
                  length(human[["selected"]]) == 0L ~
                    "missing_human_predictor",
                  length(vec_climate) == 0L ~ "missing_climate_predictor",
                  .default = "eligible_for_design_check"
                )

              vec_errors <-
                c(
                  human[["error_message"]],
                  data_unit_row[["climate_error"]]
                ) |>
                stats::na.omit()

              selection_error <-
                if (
                  length(vec_errors) == 0L
                ) {
                  NA_character_
                } else {
                  stringr::str_c(vec_errors, collapse = "; ")
                }

              dplyr::bind_cols(
                data_unit_row |>
                  dplyr::select(dplyr::all_of(unit_columns)),
                model_specifications[
                  index_spec,
                  c("model_id", "model_label", "exploratory")
                ],
                tibble::tibble(
                  selection_status = selection_status,
                  selection_error = selection_error,
                  n_rows_without_human = as.integer(n_without_human),
                  climate_set = data_unit_row[["climate_set"]],
                  data_merge = list(data_unit_model),
                  predictor_vars = list(
                    list(
                      human = human[["selected"]],
                      climate = vec_climate
                    )
                  ),
                  selection_audit = list(
                    dplyr::bind_rows(
                      data_unit_row[["climate_audit"]][[1]],
                      human[["audit"]]
                    )
                  )
                )
              )
            }
          ) |>
          dplyr::bind_rows()
      }
    ) |>
    dplyr::bind_rows()

  return(res)
}
