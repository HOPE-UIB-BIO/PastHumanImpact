#' @title Prepare local HVarPart designs with the full climate block
#' @description
#' Replace the selected climate block of chosen models in a local design
#' table with every valid climate candidate, so that a full-climate model can
#' be compared with the filtered selection on identical data.
#' @param data_designs
#' Output of `prepare_local_hvarpart_model_designs()`.
#' @param model_ids
#' Character vector of `model_id` values that receive the full climate block.
#' @param climate_candidates
#' Character vector of climate predictor names.
#' @param data_col
#' Name of the list column holding the nested data frames.
#' @return
#' `data_designs` with `predictor_vars` and `selection_status` updated for the
#' rows of `model_ids`, and a column `climate_representation` that is
#' `"full"` for those rows and `"selected"` for all other rows.
#' @details
#' A climate candidate is valid in a unit when the column exists, every value
#' is finite, and it has more than one distinct value; this matches the
#' column rule of the temporal and spatial HVarPart design checks. Human
#' predictors, nested data, and selection audits are left unchanged. For the
#' updated rows, a human-side status (`human_selection_error`,
#' `missing_human_predictor`) is kept; otherwise the status becomes
#' `missing_climate_predictor` when no candidate is valid and
#' `eligible_for_design_check` when at least one is.
#' @examples
#' \dontrun{
#' prepare_full_climate_hvarpart_designs(
#'   data_designs = designs,
#'   model_ids = "spd__full_climate"
#' )
#' }
prepare_full_climate_hvarpart_designs <- function(
  data_designs,
  model_ids,
  climate_candidates = c(
    "temp_annual", "temp_cold", "prec_summer", "prec_win"
  ),
  data_col = "data_merge"
) {
  assertthat::assert_that(
    is.data.frame(data_designs),
    assertthat::is.string(data_col),
    all(c(
      "model_id", "selection_status", data_col, "predictor_vars"
    ) %in% names(data_designs)),
    is.list(data_designs[[data_col]]),
    is.list(data_designs[["predictor_vars"]]),
    is.character(model_ids),
    length(model_ids) > 0L,
    all(model_ids %in% data_designs[["model_id"]]),
    is.character(climate_candidates),
    length(climate_candidates) > 0L,
    msg = "Full-climate design inputs do not satisfy the contract."
  )

  vec_human_statuses <-
    c("human_selection_error", "missing_human_predictor")

  flag_full <-
    data_designs[["model_id"]] %in% model_ids

  list_updated <-
    list(
      data_designs[[data_col]],
      data_designs[["predictor_vars"]],
      data_designs[["selection_status"]],
      flag_full
    ) |>
    purrr::pmap(
      .f = ~ {
        data_unit <- ..1
        predictor_vars <- ..2
        selection_status <- ..3
        flag_row_full <- ..4

        if (
          !isTRUE(flag_row_full)
        ) {
          return(list(
            predictor_vars = predictor_vars,
            selection_status = selection_status
          ))
        }

        vec_valid_climate <-
          climate_candidates |>
          purrr::keep(
            .p = ~ .x %in% names(data_unit) &&
              is.numeric(data_unit[[.x]]) &&
              all(is.finite(data_unit[[.x]])) &&
              dplyr::n_distinct(data_unit[[.x]]) > 1L
          )

        predictor_vars[["climate"]] <- vec_valid_climate

        selection_status_full <-
          dplyr::case_when(
            selection_status %in% vec_human_statuses ~ selection_status,
            length(vec_valid_climate) == 0L ~ "missing_climate_predictor",
            .default = "eligible_for_design_check"
          )

        list(
          predictor_vars = predictor_vars,
          selection_status = selection_status_full
        )
      }
    )

  res <-
    data_designs |>
    dplyr::mutate(
      predictor_vars = purrr::map(list_updated, "predictor_vars"),
      selection_status = purrr::map_chr(list_updated, "selection_status"),
      climate_representation = dplyr::if_else(
        flag_full,
        "full",
        "selected"
      )
    )

  return(res)
}
