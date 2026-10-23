#' @title Summarise full spatial HVarPart diagnostics after dbMEM selection
#' @description Reconstruct each eligible final design and report correlations,
#' VIFs, condition indices, rank, and residual degrees of freedom with selected
#' dbMEM variables retained as protected controls.
#' @param data_results Locally fitted spatial model table.
#' @param response_vars Response columns used to determine complete fit rows.
#' @return A named list of full-design diagnostic tables.
#' @examples
#' \dontrun{summarise_spatial_control_collinearity_diagnostics(fits, responses)}
summarise_spatial_control_collinearity_diagnostics <- function(
  data_results,
  response_vars
) {
  eligible_status <- c("spatial_model_estimated", "no_spatial_terms_selected")
  output <- purrr::map(seq_len(nrow(data_results)), .f = ~ {
    index <- .x
    result <- data_results$result[[index]]
    if (!result$status %in% eligible_status) return(NULL)
    data_group <- data_results$data_merge[[index]]
    predictor_vars <- data_results$predictor_vars[[index]]
    focal <- unlist(predictor_vars, use.names = FALSE)
    required <- unique(c("dataset_id", "long", "lat", response_vars, focal))
    complete <- stats::complete.cases(data_group[required])
    data_model <- data_group[complete, , drop = FALSE]
    selected_controls <- result$selection$selected_names
    if (length(selected_controls) > 0L) {
      basis <- result$dbmem$basis[selected_controls]
      assertthat::assert_that(
        nrow(basis) == nrow(data_model),
        msg = "Selected dbMEM rows do not align with the final spatial design."
      )
      data_model <- dplyr::bind_cols(data_model, basis)
    }
    diagnostic <- diagnose_local_hvarpart_collinearity(
      data_source = data_model,
      predictor_vars = predictor_vars,
      candidate_vars = focal,
      control_vars = selected_controls
    )
    identifiers <- data_results[index, c("region", "age", "model_id"), drop = FALSE]
    list(
      correlations = prepare_hvarpart_diagnostic_identifiers(
        diagnostic$correlations |>
          dplyr::filter(.data[["stage"]] == "after_selection_with_controls"),
        identifiers
      ),
      vif = prepare_hvarpart_diagnostic_identifiers(
        diagnostic$vif |>
          dplyr::filter(.data[["stage"]] == "after_selection_with_controls"),
        identifiers
      ),
      condition_indices = prepare_hvarpart_diagnostic_identifiers(
        diagnostic$condition_indices |>
          dplyr::filter(.data[["stage"]] == "after_selection_with_controls"),
        identifiers
      ),
      design = prepare_hvarpart_diagnostic_identifiers(
        diagnostic$design |>
          dplyr::filter(.data[["stage"]] == "after_selection_with_controls"),
        identifiers
      )
    )
  }) |>
    purrr::compact()
  table_names <- c("correlations", "vif", "condition_indices", "design")
  res <- purrr::map(table_names, ~ purrr::map_dfr(output, .x)) |>
    rlang::set_names(table_names)

  return(res)
}
