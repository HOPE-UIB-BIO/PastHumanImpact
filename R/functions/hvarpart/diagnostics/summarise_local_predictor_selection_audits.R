#' @title Summarise audits for local predictor selection
#' @description Flatten per-unit selection provenance and calculate local
#' correlation, VIF, condition-index, rank, and residual-df diagnostics before
#' and after selection, optionally including protected time controls.
#' @param data_designs Local design table.
#' @param unit_columns Analytical-unit identifier columns.
#' @param include_time Whether to add scaled age as a protected control.
#' @param candidate_vars Complete focal predictor candidate set.
#' @return A named list of selection, frequency, correlation, VIF,
#' condition-index, and design tables.
#' @examples
#' \dontrun{summarise_local_predictor_selection_audits(designs, "dataset_id")}
summarise_local_predictor_selection_audits <- function(
  data_designs,
  unit_columns,
  include_time = FALSE,
  candidate_vars = c(
    "spd_raw", "spd_sqrt", "kk10_fraction", "hyde_raw", "hyde_sqrt",
    "temp_annual", "temp_cold", "prec_summer", "prec_win"
  )
) {
  assertthat::assert_that(
    is.data.frame(data_designs),
    all(c(unit_columns, "model_id", "data_merge", "predictor_vars", "selection_audit") %in% names(data_designs)),
    msg = "Local predictor audit inputs do not satisfy the contract."
  )
  output <- purrr::map(seq_len(nrow(data_designs)), .f = ~ {
    index <- .x
    identifiers <- data_designs[index, c(unit_columns, "model_id"), drop = FALSE]
    data_unit <- data_designs$data_merge[[index]]
    controls <- character()
    valid_time <-
      "age" %in% names(data_unit) &&
      all(is.finite(data_unit[["age"]])) &&
      dplyr::n_distinct(data_unit[["age"]]) > 1L
    if (isTRUE(include_time) && valid_time) {
      data_unit <- scale_temporal_age(data_unit, age_col = "age", output_col = "time")
      controls <- "time"
    }
    diagnostics <- diagnose_local_hvarpart_collinearity(
      data_source = data_unit,
      predictor_vars = data_designs$predictor_vars[[index]],
      candidate_vars = candidate_vars,
      control_vars = controls
    )
    list(
      selection = prepare_hvarpart_diagnostic_identifiers(
        data_designs$selection_audit[[index]], identifiers
      ),
      correlations = prepare_hvarpart_diagnostic_identifiers(
        diagnostics$correlations, identifiers
      ),
      vif = prepare_hvarpart_diagnostic_identifiers(
        diagnostics$vif, identifiers
      ),
      condition_indices = prepare_hvarpart_diagnostic_identifiers(
        diagnostics$condition_indices, identifiers
      ),
      design = prepare_hvarpart_diagnostic_identifiers(
        diagnostics$design, identifiers
      )
    )
  })
  names_to_bind <- c("selection", "correlations", "vif", "condition_indices", "design")
  result <- purrr::map(names_to_bind, .f = ~ purrr::map_dfr(output, .x)) |>
    rlang::set_names(names_to_bind)
  result$selection_frequencies <- result$selection |>
    dplyr::count(
      dplyr::across(dplyr::any_of(c(
        "region", "model_id", "group", "predictor", "selected", "reason"
      ))),
      name = "n_units"
    )
  return(result)
}
