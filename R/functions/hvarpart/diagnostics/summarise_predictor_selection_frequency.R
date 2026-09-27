#' @title Summarise frequencies of local predictor retention
#' @description
#' Calculate the number and proportion of eligible datasets retaining each
#' candidate predictor within a requested geographic grouping.
#' @param selection_records Long predictor-selection records joined to
#' geographic metadata.
#' @param grouping_variable Name of the geographic grouping column.
#' @return A data frame with candidate-specific dataset counts and retention
#' rates within each geographic group.
#' @examples
#' \dontrun{
#' summarise_predictor_selection_frequency(selection_records, "region")
#' }
summarise_predictor_selection_frequency <- function(
  selection_records,
  grouping_variable
) {
  assertthat::assert_that(
    is.data.frame(selection_records),
    is.character(grouping_variable), length(grouping_variable) == 1L,
    grouping_variable %in% names(selection_records),
    all(c(
      "dataset_id", "group", "predictor", "selected"
    ) %in% names(selection_records)),
    msg = "Predictor-selection frequency inputs do not satisfy the contract."
  )
  res <- selection_records |>
    dplyr::summarise(
      n_datasets = dplyr::n_distinct(.data[["dataset_id"]]),
      n_selected = sum(.data[["selected"]]),
      retention_rate = mean(.data[["selected"]]),
      .by = dplyr::all_of(c(grouping_variable, "group", "predictor"))
    ) |>
    dplyr::arrange(
      .data[[grouping_variable]], .data[["group"]],
      .data[["predictor"]]
    )
  return(res)
}
