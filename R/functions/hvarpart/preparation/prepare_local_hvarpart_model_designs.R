#' @title Prepare locally selected HVarPart model designs
#' @description Expand analytical units across human-proxy specifications,
#' selecting climate once per unit and reusing it unchanged across variants.
#' @param data_source Nested analytical-unit data.
#' @param model_specifications Human-proxy model specification table.
#' @param unit_columns Columns identifying an analytical unit.
#' @param data_col Nested data column.
#' @param climate_candidates Climate candidate names in preference order.
#' @param max_cor Maximum within-group absolute correlation.
#' @param max_vif Maximum within-group VIF.
#' @return A tibble with one row per unit and model, selected predictor lists,
#'   selection status, explicit selection errors, and complete audit tables.
#' @examples
#' \dontrun{
#' prepare_local_hvarpart_model_designs(nested, specs, "dataset_id")
#' }
prepare_local_hvarpart_model_designs <- function(
  data_source,
  model_specifications,
  unit_columns,
  data_col = "data_merge",
  climate_candidates = c(
    "temp_annual", "temp_cold", "prec_summer", "prec_win"
  ),
  max_cor = 0.8,
  max_vif = 5
) {
  assertthat::assert_that(
    is.data.frame(data_source),
    all(c(unit_columns, data_col) %in% names(data_source)),
    is.list(data_source[[data_col]]),
    is.data.frame(model_specifications),
    all(c(
      "model_id", "model_label", "human_candidates", "human_preference",
      "exploratory"
    ) %in% names(model_specifications)),
    msg = "Local HVarPart design inputs do not satisfy the contract."
  )

  unit_rows <- purrr::map_dfr(seq_len(nrow(data_source)), .f = ~ {
    index <- .x
    data_unit <- data_source[[data_col]][[index]]
    climate <- select_local_hvarpart_predictor_group(
      data_source = data_unit,
      candidates = climate_candidates,
      preference = climate_candidates,
      group_name = "climate",
      max_cor = max_cor,
      max_vif = max_vif
    )
    climate_selected <- climate$selected
    climate_audit <- climate$audit

    purrr::map_dfr(seq_len(nrow(model_specifications)), .f = ~ {
      spec_index <- .x
      human_candidates <- model_specifications[["human_candidates"]][[spec_index]]
      human_preference <- model_specifications[["human_preference"]][[spec_index]]
      human <- select_local_hvarpart_predictor_group(
        data_source = data_unit,
        candidates = human_candidates,
        preference = human_preference,
        group_name = "human",
        max_cor = max_cor,
        max_vif = max_vif
      )
      predictor_vars <- list(
        human = human$selected,
        climate = climate_selected
      )
      status <- dplyr::case_when(
        human$status == "selection_error" ~ "human_selection_error",
        climate$status == "selection_error" ~ "climate_selection_error",
        length(predictor_vars$human) == 0L ~ "missing_human_predictor",
        length(predictor_vars$climate) == 0L ~ "missing_climate_predictor",
        .default = "eligible_for_design_check"
      )
      selection_error <- c(
        human$error_message,
        climate$error_message
      ) |>
        stats::na.omit() |>
        paste(collapse = "; ")
      if (identical(selection_error, "")) selection_error <- NA_character_
      identifiers <- data_source[index, unit_columns, drop = FALSE]
      dplyr::bind_cols(
        identifiers,
        model_specifications[spec_index, c("model_id", "model_label", "exploratory")],
        tibble::tibble(
          selection_status = status,
          selection_error = selection_error,
          data_merge = list(data_unit),
          predictor_vars = list(predictor_vars),
          selection_audit = list(dplyr::bind_rows(
            climate_audit,
            human$audit
          ))
        )
      )
    })
  })

  return(unit_rows)
}
