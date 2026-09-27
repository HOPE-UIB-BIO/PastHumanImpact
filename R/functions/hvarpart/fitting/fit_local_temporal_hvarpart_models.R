#' @title Fit time-controlled HVarPart models with local selections
#' @description Call the existing single-dataset temporal fitter with the
#' predictor groups selected for each analytical unit and retain selections and
#' classified failures beside every result.
#' @param data_designs Expanded local design table.
#' @param response_vars Response variable names.
#' @param seed Base random seed.
#' @param ... Arguments passed to `fit_temporal_hvarpart_dataset()`.
#' @return The design identifiers, selections, result objects, and error text.
#' @examples
#' \dontrun{
#' fit_local_temporal_hvarpart_models(designs, responses)
#' }
fit_local_temporal_hvarpart_models <- function(
  data_designs,
  response_vars,
  seed = 1234L,
  ...
) {
  required <- c(
    "dataset_id", "model_id", "selection_status", "data_merge",
    "predictor_vars", "selection_audit"
  )
  assertthat::assert_that(
    is.data.frame(data_designs), all(required %in% names(data_designs)),
    msg = "Local temporal model designs do not satisfy the contract."
  )
  extra <- rlang::list2(...)
  res <- data_designs |>
    dplyr::mutate(
      .row_index = dplyr::row_number(),
      fit = purrr::pmap(
        list(.data[["data_merge"]], .data[["predictor_vars"]],
             .data[["selection_status"]], .data[[".row_index"]]),
        .f = ~ {
          data_merge <- ..1
          predictor_vars <- ..2
          selection_status <- ..3
          .row_index <- ..4
          if (selection_status != "eligible_for_design_check") {
            return(list(
              result = build_empty_temporal_hvarpart_result(
                status = selection_status,
                n_samples = nrow(data_merge)
              ),
              error_message = NA_character_
            ))
          }
          fitted <- rlang::exec(
            purrr::safely(fit_temporal_hvarpart_dataset),
            data_dataset = data_merge,
            response_vars = response_vars,
            predictor_vars = predictor_vars,
            seed = as.integer(seed + .row_index),
            !!!extra
          )
          if (!is.null(fitted$error)) {
            return(list(
              result = build_empty_temporal_hvarpart_result(
                status = "model_error",
                n_samples = nrow(data_merge)
              ),
              error_message = conditionMessage(fitted$error)
            ))
          }
          list(result = fitted$result, error_message = NA_character_)
        }
      ),
      result = purrr::map(.data[["fit"]], "result"),
      error_message = purrr::map_chr(.data[["fit"]], "error_message")
    ) |>
    dplyr::select(-dplyr::all_of(c(".row_index", "fit")))

  return(res)
}
